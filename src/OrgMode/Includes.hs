{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module       : OrgMode.Includes
Description  : Handling of #+INCLUDE directives in parsed org-mode documents
Copyright    : (c) 2026, Kevin Quick
License      : BSD3
Stability    : provisional
Portability  : portable

Org-mode documents can include a sub-document via the #+INCLUDE: directive in the
org-mode document.  The Daedalus parsing of the top-level document is a pure
operation that does not provide the ability to perform I/O to read included
documents.  Instead, this module provides a function to locate and process these
directives with sub-parsing of the included org-mode documents.

Note that included org-mode documents may themselves have inclusions, which means
the number and location of inclusions is dynamic.  The functionality in this
document is therefore iterative: the first include directive is located and
processed, returning the result, and then the process is repeated until there are
no more include directives.

If not processed, the original #+INCLUDE directives remain in the OrgDoc;
different output modes may or may not display the include directive in some form,
but none of them will actually include the target file.

-}

module OrgMode.Includes
  (
    insertAllInclusions
  , beginInclusionProcessing
  , insertOrgModeDocument
  , OrgIDoc ( orgDoc, includeFile )
  , InclusionStatus(..)
  )
where

import           Control.Lens hiding ( pre, re )
import qualified Daedalus.RTS.Numeric as DN
import qualified Daedalus.RTS.Vector as DV
import           Data.Char ( ord )
import           Data.List ( genericSplitAt )
import           Data.Maybe ( catMaybes )
import           Data.Name
import           Data.String ( fromString )
import           Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Error.Diagnose
import           GHC.Records ( HasField, getField )
import           Numeric.Natural
import           OrgMode.Parse
import           Path
import qualified Path.IO as PIO
import           Text.Read ( readMaybe )
import           Text.Sayable


-- | This data structure maintains the context for processing inclusions in an
-- OrgDoc.  It is updated as each include directive is processed.  When
-- processing is completed, the resulting document may be extracted by the orgDoc
-- field accessor, and at each step the current include file to process is
-- specified by the includeFile element (the caller may change this field to
-- redirect the inclusion if desired).
data OrgIDoc = OrgIDoc { orgDoc :: OrgDoc
                         -- ^ Specifies the document as updated by each inclusion
                         -- directive processing.  When there are no more include
                         -- directives, this represents the final form of the
                         -- org-mode document.
                       , includeFile :: Maybe (Name "org-mode include filename")
                         -- ^ Specifies the current inclusion directive's target
                         -- file path.  This will initially be set to the first
                         -- include directive location via
                         -- 'beginInclusionProcessing' and updated by each
                         -- 'insertOrgModeDocument' which removes the current
                         -- inclusion directive and updates to point to the next
                         -- include directive.  This will be Nothing when there
                         -- are no more include directives.  It may be updated to
                         -- redirect the include path.
                       , _internal :: OrgIDocInternal
                         -- ^ Internal context maintained for processing the
                         -- document.  This information is internal to this
                         -- module and is not accessible by callers.
                       }

data OrgIDocInternal = OIDI { _location :: Maybe Location
                              -- Nothing means no more include settings in the
                              -- file.
                            }

-- Location is where we can find the include directive, which is a sequence of
-- section indices terminating in an count of the include setting instances in
-- the body.  Normally include directives are replaced with their content, so
-- this is usually the first include setting in the entire OrgDoc, but this does
-- allow skipping include settings.  The SOF value is the special value for
-- "start of file" indicating no include has been located, and is treated as
-- being either the first include setting or the end of all include settings.
data Location = Section Natural Location
                -- ^ In the Natural numbered section, continue with Location
                -- within that section.
              | InclSettingNum Natural
                -- ^ The Natural numbered include in the current body
              -- | DocSettingNum Natural
              --   -- ^ Like InclSettingNum, but this is for the initial body of an
              --   -- OrgDoc, which is always relative to the top level of the
              --   -- document.
              | InclSettingNumAt Natural Natural
                -- ^ Like InclSettingNum, but the second Natural indicates the
                -- level at which this setting should be added.
              deriving Eq

instance Sayable saytag Location where
  sayable = \case
    Section n l -> 'S' &+ n &+ '.' &+ l
    InclSettingNum n -> '#' &+ n
    InclSettingNumAt n l -> '#' &+ n &+ '(' &+ '@' &+ l &+ ')'

makeLenses ''OrgIDoc
makeLenses ''OrgIDocInternal


pattern IncludeDirective :: OrgMode.Parse.Setting -> OrgBody
pattern IncludeDirective s <- OrgBody_setting s@(getKW -> "INCLUDE")


getKW :: OrgMode.Parse.Setting -> Named CaseInsensitive "keyword"
getKW = fromString . DV.vecToString . getField @"keyword"


-- | Creates a working inclusion document data structure that can be used to find
-- and process all the include directives in the target file.
--
-- The provided FilePath is the *directory* in which the original document is
-- located; this is used for processing relative include specifications.

beginInclusionProcessing :: OrgDoc -> OrgIDoc
beginInclusionProcessing d =
  let idoc = OrgIDoc { orgDoc = d
                     , includeFile = Nothing
                     , _internal =
                         OIDI { _location = Nothing -- replaced immediately
                              }
                     }
  in setNextInclude idoc $ docIncludes d


setNextInclude :: OrgIDoc -> DocIncludesInfo -> OrgIDoc
setNextInclude d = \case
  ((i,l):_) -> d { includeFile = Just i }
               & internal . location .~ Just l
  [] -> d { includeFile = Nothing }
        & internal.location .~ Nothing


type DocIncludesInfo = [(Name "org-mode include filename", Location)] -- KWQ: TODO" return minlevel

docIncludes :: OrgDoc -> DocIncludesInfo
docIncludes d =
  let bodyIncludes :: HasField "body" p (DV.Vector OrgBody)
                   => IsOrgDoc p
                   => p -> [(Name "org-mode include filename", Location)]
      bodyIncludes p =
        fmap (\(n,f) -> (inclFileSpec f
                        , case inclMinLevel f of
                                    Just l -> InclSettingNumAt n l
                                    Nothing ->
                                      if isOrgDoc p
                                      then InclSettingNumAt n 0
                                      else InclSettingNum n))
                       $ zip [0..]
                       $ catMaybes
                       (bodyInclude <$> (DV.toList $ getField @"body" p))
      bodyInclude :: OrgBody -> Maybe IncludeInfo
      bodyInclude = includeInfo
      sectionBodyIncludes :: (Natural, OrgSection) -> [Maybe (Name "org-mode include filename", Location)]
      sectionBodyIncludes (n,p) =
        (fmap (Just . fmap (Section n)) $ bodyIncludes p)
        <> (fmap (fmap (fmap (Section n))) $ sectionIncludes p)
      sectionIncludes :: HasField "sections" p (DV.Vector OrgSection)
                      => p -> [Maybe (Name "org-mode include filename", Location)]
      sectionIncludes p = concatMap sectionBodyIncludes
                          (zip [0..] $ DV.toList $ getField @"sections" p)
  in bodyIncludes d <> catMaybes (sectionIncludes d)


-- | Inserts the new sub-document into the inclusion document at the current
-- include directive's position.  The returned inclusion document will have the
-- inclusion directive removed and the location will be advanced to the next
-- inclusion directive (if any, and possibly in the newly inserted portion).

insertOrgModeDocument :: OrgDoc -> OrgIDoc -> OrgIDoc
insertOrgModeDocument subdoc maindoc =
  -- KWQ Assumes internal . location is valid (i.e. not Nothing and not Just SOF)
  let loc = case maindoc ^. internal . location of
              Just l -> l
              Nothing ->
                error $ sez @"error"
                $ t'"Attempted to insert when there is no current insertion point"

      subdoc' = elevateDocBodyIncludes subdoc

      go :: HasField "body" p (DV.Vector OrgBody)
         => HasField "sections" p (DV.Vector OrgSection)
         => SetBodySections p
         => GetLevel p
         => DeferPlacement p
         => Natural -> Location -> p
         -> Either (([OrgBody], [OrgSection]), (OrgSection, Natural)) p
      go _ (InclSettingNum n) p =
        let (bdy', secs') = inclSettingInBody n p
        in Right $ setBodySections bdy' secs' p

      -- OrgBody includes are relative to the level of the section they are in
      -- OrgDoc includes are always at level 0
      -- Multiple includes are always relative to each other

      -- Case 1: include in a subsection
      --            include is relative to the subsection
      --
      -- Case 1:
      --    body
      --    include1
      --    text1
      --    include2
      --    text2
      --
      -- text1 is appended to include1's end, include2 is at the same level as
      -- include1, and text2 is appended to include2's end

      go depth (InclSettingNumAt n l) p =
        let bdy = DV.toList $ getField @"body" p
            secs = DV.toList $ getField @"sections" p
            lvl = getLevel p
            replN n' (b:bs) =
              case b of
                IncludeDirective _ | n' == 0 ->
                  if depth == l - 1
                  then Right (insertHere bs secs ((+) lvl) subdoc')
                  else
                    let sbody = DV.toList $ getField @"body" subdoc'
                    in Left (sbody, bs, secs)
                _ ->
                  case replN n' bs of
                    Right r -> Right r
                    Left (bsub, replb, repls) -> Left (b:bsub, replb, repls)
            replN _ [] =
              -- This should never happen because the first argument should
              -- reference an existing include directive in this body; if it
              -- does, just drop the include document in here.
              Right (insertHere [] secs ((+) lvl) subdoc')
        in case replN n bdy of
          Right (bdy', secs') -> Right $ setBodySections bdy' secs' p
          Left (bsub, replb, repls) ->
              deferPlacement (l - 1) p bsub mempty replb repls

      go d (Section n l') p =
        let bdy = DV.toList $ getField @"body" p
            secs = DV.toList $ getField @"sections" p
            (pre, here) = genericSplitAt n secs
        in case uncons here of
             Just (sec, post) ->
               case go (succ d) l' sec of
                 Left ((b', ss), (sec', dtgt)) ->
                   if d == dtgt
                   then
                     let (nb,ns) = insertHere b' mempty ((+) dtgt)
                                   $ setBodySections mempty nsubs subdoc'
                         sdsubs = DV.toList $ getField @"sections" subdoc'
                         nsubs = insertSecsAtBottomEnd id sdsubs ss <> post
                     in Right $ setBodySections (bdy <> nb) (pre <> (sec' : ns)) p
                   else deferPlacement dtgt p bdy (pre <> [sec'] <> post) b' ss
                 Right s' -> Right $ setBodySections bdy (pre <> [s'] <> post) p
             Nothing -> error "blah" -- KWQ TODO

      insertSecsAtBottomEnd :: (Natural -> Natural) -> [OrgSection]
                            -> [OrgSection]
                            -> [OrgSection]
      insertSecsAtBottomEnd offs (reverse -> (s:ss)) insecs =
          case DV.toList $ getField @"sections" s of
            [] -> reverse (s:ss) <> (updateSecLevels offs insecs)
            subs ->
              let subs' = insertSecsAtBottomEnd (succ . offs) subs insecs
                  bdy = DV.toList $ getField @"body" s
              in reverse ((setBodySections bdy subs' s) : ss)
      insertSecsAtBottomEnd _ _ insecs = insecs

      inclSettingInBody n p =
        let bdy = DV.toList $ getField @"body" p
            secs = DV.toList $ getField @"sections" p
            lvl = getLevel p
            replN n' (b:bs) ss =
              case b of
                IncludeDirective _ | n' == 0 ->
                  insertHere bs ss ((+) lvl) subdoc'
                _ -> let (b', s') = replN n' bs ss in (b:b', s')
            replN _ [] ss =
              -- This should never happen because the first argument should
              -- reference an existing include directive in this body; if it
              -- does, just drop the include document in here.
              insertHere [] ss ((+) lvl) subdoc'
        in replN n bdy secs

      insertHere :: HasField "body" p (DV.Vector OrgBody)
                 => HasField "sections" p (DV.Vector OrgSection)
                 => SetBodySections p
                 => GetLevel p
                 => [OrgBody]
                 -> [OrgSection]
                 -> (Natural -> Natural)
                 -> p
                 -> ([OrgBody], [OrgSection])
      insertHere bs secs updLevel what =
        let nb = DV.toList $ getField @"body" what
            ns = DV.toList $ getField @"sections" what
        in case reverse ns of
             [] -> (nb <> bs, updateSecLevels updLevel secs)
             (sl:os) ->
               let (a, b) = insertHere bs secs (succ . updLevel) sl
                   sl' = updateSectionLevel updLevel $ setBodySections a b sl
               in (nb, reverse (sl' : updateSecLevels updLevel os))

      updateSecLevels :: (Natural -> Natural) -> [OrgSection] -> [OrgSection]
      updateSecLevels f ss = updateSecLevel f <$> ss

      updateSecLevel f s =
        let b = DV.toList $ getField @"body" s
            ss = DV.toList $ getField @"sections" s
            ss' = updateSecLevels (succ . f) ss
        in updateSectionLevel f $ setBodySections b ss' s

      newDoc = case go 0 loc (orgDoc maindoc) of
                 Right nd -> nd
                 Left _ -> error "Did not find right level for insertion!"
      newIDoc = maindoc { orgDoc = newDoc }

      -- Find the next include point *with* the new inserted document, which
      -- itself might contain include directives.  This is not necessarily the
      -- first include in the document (although it usually is) because some
      -- might have been skipped.  The technique used here is to recognize that
      -- since the current include directive is going to be removed, the new one
      -- should be at the same offset in the includes contained in the new
      -- document (i.e. the same number of include directives skipped in the
      -- original document should be skipped in the new document).
      --
      -- Note that the current body is split at the inclusion point: the new
      -- content body continues the current body, then the new sections are
      -- inserted, and then the remainder of the original body is appended to the
      -- body of the last section inserted. This does not change the fact that
      -- the next inclusion is at the same current index when checking the new
      -- document.
      origIncludes = docIncludes (orgDoc maindoc)
      newIncludesAll = docIncludes (orgDoc newIDoc)
      curIncludeIdx =
        case dropWhile ((/=) loc . snd . snd) $ zip [0..] origIncludes of
          (i:_) -> fst i
          [] -> error $ sez @"error"
                $ t'"Current include location" &- loc &- t'"not found!"
      newIncludes =
        if null origIncludes -- unlikely: we just did one!
        then newIncludesAll
        else drop (curIncludeIdx - 1) newIncludesAll

  in setNextInclude newIDoc newIncludes

class SetBodySections a where
  setBodySections :: [OrgBody] -> [OrgSection] -> a -> a
instance SetBodySections OrgDoc where
  setBodySections b s _ = OrgDoc (DV.fromList b) (DV.fromList s)
instance SetBodySections OrgSection where
  setBodySections b s (OrgSection l t p h _ _) =
    OrgSection l t p h (DV.fromList b) (DV.fromList s)

class GetLevel a where
  getLevel :: a -> Natural
instance GetLevel OrgDoc where getLevel = const 0
instance GetLevel OrgSection where getLevel = uint8ToNatural . getField @"level"

class DeferPlacement a where
  deferPlacement :: Natural -> a
                 -> [OrgBody] -> [OrgSection]
                 -> [OrgBody] -> [OrgSection]
                 -> Either (([OrgBody], [OrgSection]), (OrgSection, Natural)) a
instance DeferPlacement OrgDoc where
  deferPlacement = error "Cannot defer level at the doc top level"
instance DeferPlacement OrgSection where
  deferPlacement l p bsub ssub replb repls =
    let p' = setBodySections bsub ssub p
    in Left ((replb, repls), (p', l))

elevateDocBodyIncludes :: OrgDoc -> OrgDoc
elevateDocBodyIncludes inDoc =
  let b = DV.toList $ getField @"body" inDoc
      b' = elevateInclude <$> b
      elevateInclude obe =
        case includeInfo obe of
          Just ii | Nothing <- inclMinLevel ii ->
            -- Convert this back into an OrgBodyElement Setting with a minlevel
            -- specified.
                    let inclDirective = stringToDVec "include"
                        tgtFileSpec = stringToDVec (T.unpack $ nameText
                                                    $ inclFileSpec ii)
                        minLev = stringToDVec ":minlevel"
                        levstr = stringToDVec $ show (1 :: Natural)
                        args = DV.fromList [ tgtFileSpec, minLev, levstr ]
                    in
                      OrgBody_setting (Setting inclDirective args)
          _ -> obe
  in setBodySections b' (DV.toList $ getField @"sections" inDoc) inDoc


stringToDVec :: String -> DV.Vector (DN.UInt 8)
-- stringToDVec = DV.vecFromRep . BS8.pack
-- stringToDVec = DV.vecFromRep . DV.fromList
stringToDVec = DV.fromList . fmap (DN.toUInt . toEnum . ord)

data IncludeInfo = IncludeInfo { inclFileSpec :: Name "org-mode include filename"
                               , inclMinLevel :: Maybe Natural
                               }

includeInfo :: OrgBody -> Maybe IncludeInfo
includeInfo = \case
  IncludeDirective s ->
    case DV.vecToString <$> (DV.toList $ getField @"values" s) of
      [] -> error "NO VALUE for include directive" -- KWQ TODO
      (incfile:vals) ->
        let what [] = Nothing
            what (kw:lvl:svs) | minLevelKW == kw =
                                          case readMaybe $ T.unpack $ nameText lvl of
                                           Nothing -> what $ lvl:svs
                                           Just l -> Just l
            what (_:svs) = what svs
            minLevelKW = ":minlevel"
                         :: Named CaseInsensitive "include directive argument"
            strVals = fromString <$> vals
        in Just $ IncludeInfo (fromString incfile) (what strVals)
  _ -> Nothing


class IsOrgDoc a where isOrgDoc :: a -> Bool
instance IsOrgDoc OrgDoc where isOrgDoc = const True
instance IsOrgDoc OrgSection where isOrgDoc = const False

updateSectionLevel :: (Natural -> Natural) -> OrgSection -> OrgSection
updateSectionLevel f (OrgSection l t p h b s) =
  OrgSection (naturalToUInt8 $ f $ uint8ToNatural l) t p h b s

uint8ToNatural :: DN.UInt 8 -> Natural
uint8ToNatural = toEnum . fromEnum . DN.fromUInt

naturalToUInt8 :: Natural -> DN.UInt 8
naturalToUInt8 = DN.uint8 . toEnum . fromEnum


----------------------------------------------------------------------


-- | Indicates the processed status of each file specified in an include
-- directive.

data InclusionStatus
  = Included (SomeBase File) -- ^ Successfully included this file
  | NotFound (Name "org-mode include filename") (SomeBase File)
    -- ^ File was not found.  The first argument is the specific path provided by
    -- the directive itself and the second argument is the actual path where the
    -- file was looked for.
    --
    -- The include directive is removed and replaced with empty text.  Processing
    -- continues for subsequent include directives.  [Note that emacs org-mode
    -- export processing will halt with an error on this condition.]
  | ParseError (SomeBase File) (Diagnostic Text)
    -- ^ The specified file failed to parse correctly by the orgmode parser.  The
    -- FilePath is the full filepath where the file was loaded (i.e. the include
    -- directive specification enhanced with any relative path specifications
    -- from the current directory).
    --
    -- The include directive is removed and replaced with empty text.  Processing
    -- continues for subsequent include directives.


instance Sayable saytag InclusionStatus where
  sayable = \case
    Included sf -> t'"included" &- show sf
    NotFound s f -> t'"path '" &+ s &+ t'"' not found (" &+ show f &+ ')'
    ParseError f d -> t'"parse error in '" &+ show f &+ t'"):" &- d


data IAI_Context = IAI { _activeIncludes :: [(SomeBase Dir, Natural)] -- KWQ: -> _activeIncludes
                       , _inclStatuses :: [InclusionStatus]
                       }


makeLenses ''IAI_Context


-- | This is a wrapper function that performs a proper iteration over all include
-- directives in the input document processing and inserting the included
-- sub-documents in the proper location.  This is normally the function used to
-- process inclusions, but if additional control is needed over inclusions
-- (e.g. skipping inclusions, etc.), the underlying beginInclusionProcessing, and
-- insertOrgModeDocument may be used explicitly.
--
-- The provided FilePath is the same directory that is passed to
-- beginInclusionProcessing.
--
-- The return value is the updated OrgDoc, along with a list of the status of all
-- inclusion operation attempts.


insertAllInclusions :: SomeBase Dir -> OrgDoc -> IO ([InclusionStatus], OrgDoc)
insertAllInclusions p0 d =
  case countIncludeDirectives d of
    0 -> return ([], d)
    n ->
      do let d2 = beginInclusionProcessing d
         let c = IAI { _activeIncludes = [(p0, n)]
                       -- Because each include file can itself have include
                       -- directives, and also because the relative paths in
                       -- include directives are relative to the file they are
                       -- included from and not relative to the top level
                       -- document, we need to dynamically track the directives
                       -- to know when to adjust the relative path for a set of
                       -- includes, and thus need to know how many directives are
                       -- in a particular document and where that document is
                       -- located.
                     , _inclStatuses = mempty -- KWQ: test these!
                     }
         go c d2
  where

    go :: IAI_Context -> OrgIDoc -> IO ([InclusionStatus], OrgDoc)
    go ctxt inpd = case includeFile inpd of
                     Nothing -> return (_inclStatuses ctxt, orgDoc inpd)
                     Just f ->
                       do let fp = actualPath ctxt f
                          isThere <- prjSomeBase PIO.doesPathExist fp
                          (ctxt', dsubs) <-
                            if isThere
                            then includeTheFile ctxt inpd fp
                            else removeInclusionDirective ctxt inpd
                                 $ NotFound f fp
                          go ctxt' dsubs

    actualPath :: IAI_Context -> Name "org-mode include filename"
               -> SomeBase File
    actualPath ctxt f =
      case parseSomeFile
           $ T.unpack
           $ T.drop 1  -- drop leading "
           $ T.reverse $ T.drop 1 $ T.reverse -- drop trailing "
           $ nameText f
      of
        Nothing -> error $ sez @"error"
                   $ t'"Unable to parse include file specification:"
                   &- f
        Just pf ->
          let refdir = maybe p0 id $ ctxt ^? activeIncludes . ix 0 . _1
          in pathMaybeRelative refdir pf

    includeTheFile :: IAI_Context -> OrgIDoc -> SomeBase File
                   -> IO (IAI_Context, OrgIDoc)
    includeTheFile ctxt inpd fp =
      do let fp' = prjSomeBase toFilePath fp
         i <- fromText <$> TIO.readFile fp'
         let opDesc = fromString "org-mode include directive"
         case orgStructureParse (fromString fp') i opDesc of
           Left e -> removeInclusionDirective ctxt inpd $ ParseError fp e
           Right sd ->
             let d2 = insertOrgModeDocument sd inpd
                 fpDir = mapSomeBase parent fp
                 nnbi = countIncludeDirectives (getField @"body" sd)
                 c' = case countIncludeDirectives sd of
                        0 -> didInclude ctxt
                        n -> didInclude ctxt & activeIncludes
                             %~ addAfterBody nnbi (fpDir, n)
             in return (c', d2)

    -- Adds an entry 'e' tracking the path of the included file and the count of
    -- the number of remaining includes in that new included body.  This is used
    -- to maintain the proper path reference for any includes made from that
    -- inserted document.
    --
    -- The tricky part is that the current body is split at the insertion point
    -- and the new document is inserted (the initial body of the new document is
    -- appended to the first part of the current body, and the remaining portion
    -- of the current body is appended to the body of the last section inserted).
    -- To support this, it's necessary to know the number of include directives
    -- in the new body to determine where to split the current activeIncludes.
    addAfterBody _ e ais = e : ais
    -- addAfterBody 0 e ais = e : ais
    -- addAfterBody n e ais =
    --   case uncons ais of
    --     Nothing -> e : ais
    --     Just ((l,c), aiss) -> (fst e, n) : (l, c)
    --                           : let re = snd e - n
    --                             in if n == 0 then aiss else (fst e, re) : aiss

    didInclude ctxt =
      if ctxt ^? activeIncludes . ix 0 . _2 == Just 1
      then ctxt & activeIncludes %~ drop 1
      else ctxt & activeIncludes . ix 0 . _2 %~ pred

    removeInclusionDirective :: IAI_Context -> OrgIDoc -> InclusionStatus
                             -> IO (IAI_Context, OrgIDoc)
    removeInclusionDirective ctxt inpd errRsn =
      let d2 = insertOrgModeDocument (OrgDoc emptyBody DV.empty) inpd
          emptyBody = DV.empty -- OrgBody True DV.empty
          ctxt' = didInclude ctxt & inclStatuses %~ (errRsn :)
      in return (ctxt', d2)


pathMaybeRelative :: SomeBase Dir -> SomeBase t -> SomeBase t
pathMaybeRelative rd rf = case rf of
                            Abs _ -> rf
                            Rel rf' -> mapSomeBase (</> rf') rd


----------------------------------------------------------------------

class IncludeDirectiveCounter a where
  -- | Returns a count of the number of include directives present in an OrgDoc.
  countIncludeDirectives :: a -> Natural

instance IncludeDirectiveCounter OrgDoc where
  countIncludeDirectives d = countIncludeDirectives (getField @"body" d)
                             + countIncludeDirectives (getField @"sections" d)

instance IncludeDirectiveCounter (DV.Vector OrgSection) where
  countIncludeDirectives sv = sum (countIncludeDirectives <$> DV.toList sv)

instance IncludeDirectiveCounter OrgSection where
  countIncludeDirectives s = countIncludeDirectives (getField @"body" s)
                             + countIncludeDirectives (getField @"sections" s)

instance IncludeDirectiveCounter (DV.Vector OrgBody) where
  countIncludeDirectives bv = sum (countIncludeDirectives <$> DV.toList bv)

instance IncludeDirectiveCounter OrgBody where
  countIncludeDirectives = \case
    IncludeDirective _ -> 1
    _ -> 0

-- REMAINING TODO:
-- 1. Include in the list didn't work (other places as well?)
-- 2. Sections have the wrong level and markers
-- 3. :minlevel is not being handled
-- 4. Oxford period.
-- emacs continues indentation for subsections, but the ~"text-style1"~ stops indenting appropriately
