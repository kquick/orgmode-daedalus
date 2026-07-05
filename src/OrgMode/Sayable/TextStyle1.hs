{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module       : OrgMode.Sayable.TextStyle1
Description  : Pretty-printing of OrgMode documents to Text/UTF-8 output
Copyright    : (c) 2026, Kevin Quick
License      : BSD3
Stability    : provisional
Portability  : portable

Sayable "text-style1" instances for org-mode ASTs, as parsed by Daedalus and
Data.OrgMode.Parse.orgParse.  This is closely equivalent to emacs' ox-text output
emitter.
-}

module OrgMode.Sayable.TextStyle1
  (
    -- exports many instances of Sayable
  )
where

import           Control.Applicative ( asum )
import           Data.Bool ( bool )
import           Data.Char ( chr, toUpper )
import qualified Data.List as L
import           Data.Tagged
import qualified Data.Text as T
import           GHC.Records ( HasField, getField )
import qualified Language.Haskell.TH as TH
import qualified Prettyprinter as PP
import           Text.Sayable

import           Daedalus.RTS hiding ( div )
import qualified Daedalus.RTS.Vector as DV
import           OrgMode.Markup
import           OrgMode.Parse
import           OrgMode.Sayable.Defs


type ReverseSectionNumbering = [Int]
-- the array of ints is the reverse order current nested section numbering

data SectionLevel a = SectionLvl
                      ReverseSectionNumbering
                      (Maybe [OrgText]) -- subtitle
                      a
  deriving Functor


slEmptyWrap :: a -> SectionLevel a
slEmptyWrap = SectionLvl mempty Nothing

slItem :: SectionLevel a -> a
slItem (SectionLvl _ _ a) = a

slAddLevel :: Int -> SectionLevel a -> SectionLevel a
slAddLevel n (SectionLvl ns st a) = SectionLvl (n:ns) st a

slLevel :: SectionLevel a -> [Int] -- forward ordered
slLevel (SectionLvl l _ _) = L.reverse l

slSubtitle :: SectionLevel a -> Maybe [OrgText]
slSubtitle (SectionLvl _ st _) = st

-- slRewrap :: a -> SectionLevel b -> SectionLevel a
-- slRewrap a (SectionLvl x y _) = SectionLvl x y a

$(return []) -- Make SectionLevel available at the Template Haskell level


-- The general implementation guideline here is that lists/vectors of items
-- intersperse a blank line between their items, but overall elements do not add
-- leading or trailing space.  One exception is that level1 headers add an extra
-- preceding space.


instance ( $(sayableSubConstraints $ do ofType ''SectionLevel
                                        tagSym "text-style1"
                                        paramTH (TH.ConT ''OrgDoc))
         ,  $(sayableSubConstraints $ do ofType ''SectionLevel
                                         tagSym "text-style1"
                                         paramTH (TH.ConT ''OrgBody))
         ,  $(sayableSubConstraints $ do ofType ''SectionLevel
                                         tagSym "text-style1"
                                         paramTH (TH.ConT ''OrgSection))
         ) => Sayable "text-style1" OrgDoc where
  sayable od =
    let getSubtitle = \case
          OrgBody_setting s
            | "SUBTITLE" == (toUpper <$> (DV.vecToString $ getField @"keyword" s))
            -> Just $ orgMarkupParseLine $ getField @"values" s
          _ -> Nothing
        subtitle = asum (getSubtitle <$> (DV.toList $ getField @"body" od))
    -- show od &<
    in if null $ DV.toList $ getField @"sections" od
       then sayable @"text-style1"
            $ SectionLvl mempty subtitle (getField @"body" od) &< ""
       else if null $ DV.toList $ getField @"body" od
            then sayable @"text-style1"
                 $ slEmptyWrap (getField @"sections" od) &< ""
            else SectionLvl mempty subtitle (getField @"body" od)
                 &< ""
                 &< slEmptyWrap (getField @"sections" od)
                 &< ""



centerLine :: Sayable "text-style1" a
           => Tagged "width" Int -> a -> Saying "text-style1"
centerLine tw a =
  let l = sez @"text-style1" a
      pfxlen = max 0 $ (unTagged tw - length l) `div` 2
  in replicate pfxlen ' ' &+ l

----------------------------------------------------------------------

instance ( Sayable "text-style1" OrgBody
         ) => Sayable "text-style1" (DV.Vector OrgBody) where
  sayable = sayable . slEmptyWrap

instance ( Sayable "text-style1" (SectionLevel OrgBody)
         , Sayable "text-style1" (DV.Vector OrgBody)
         , $(sayableSubConstraints $ do ofType ''SectionLevel
                                        tagSym "text-style1"
                                        paramTH (TH.ConT ''OrgBody))
         ) => Sayable "text-style1" (SectionLevel (DV.Vector OrgBody)) where
  sayable sl =
    let joinLines a b =
          case b of
            sl' | OrgBody_setting setting <- slItem sl' ->
              -- n.b. since some settings are not applied, break the newline
              -- general rule and be explicit here when appropriate.
              let kw = fmap toUpper $ DV.vecToString $ getField @"keyword" setting
              in if kw `elem` [ "TITLE", "AUTHOR", "EMAIL" ]
                 then a &< b
                 else a  -- drop b, it's probably blank (see Sayable OrgBody)
            _ -> withBlankLine a b
        showableBody = L.filter (not . isDrawer) $ DV.toList $ slItem sl
    in foldlNES @"text-style1" joinLines ((`fmap` sl) . const <$> showableBody)


----------------------------------------------------------------------

instance ( $(sayableSubConstraints $ ofType ''OrgSection >> tagSym "text-style1")
         , Sayable "text-style1" OrgSection
         ) => Sayable "text-style1" (DV.Vector OrgSection) where
  sayable = sayable . slEmptyWrap

instance ( Sayable "text-style1" (DV.Vector OrgSection)
         , Sayable "text-style1" (SectionLevel OrgSection)
         ) => Sayable "text-style1" (SectionLevel (DV.Vector OrgSection)) where
  sayable sl =
    let o = slItem sl
        between = case DV.toList o of
                    [] -> (&<)
                    (s:_) -> if asInt (getField @"level" s) == 1
                             then (\a b -> a &< "" &< "" &< b)
                             else withBlankLine
    in foldlNES @"text-style1" between
       $ fmap (\(n,v) -> slAddLevel n (const v <$> sl))
       $ zip [1..] $ DV.toList o

instance
  ( $(sayableSubConstraints $ ofType ''OrgSection >> tagSym "text-style1")
  ) => Sayable "text-style1" OrgSection where
  sayable = sayable . slEmptyWrap


instance
  ( $(sayableSubConstraints $ ofType ''OrgSection >> tagSym "text-style1")
  , Sayable "text-style1" OrgSection
  , Sayable "text-style1" OrgBody
  , Sayable "text-style1" (SectionLevel OrgSection)
  , Sayable "text-style1" (SectionLevel OrgBody)
  ) => Sayable "text-style1" (SectionLevel OrgSection) where
  sayable sl =
    let s = slItem sl
        tdo = addTodoAnn . DV.vecToString <$> getField @"todo" s
        addTodoAnn "TODO" = PP.annotate #todo &! "TODO"
        addTodoAnn "DONE" = PP.annotate #done &! "DONE"
        addTodoAnn o = PP.annotate #todo &! o
        hdr = let h = orgMarkupParseLine $ getField @"header" s
                  sn = sez @"text-style1" $ '.' &:* slLevel sl
                  l = maximum (displayLength <$> h) + length sn + 1
                      + maybe 0 (succ . length . DV.vecToString)
                        (getField @"todo" s)
              in case asInt (getField @"level" s) of
                   1 -> sn &? tdo &- h &< T.replicate l (T.singleton '═')
                   2 -> sn &? tdo &- h &< T.replicate l (T.singleton '─')
                   3 -> sn &? tdo &- h &< T.replicate l (T.singleton '╌')
                   _ -> '◊' &- sn &? tdo &- h
        subs = foldlNES @"text-style1" withBlankLine
               (fmap (\(n,v) -> slAddLevel n (const v <$> sl))
                $ zip [1..] $ DV.toList $ getField @"sections" s)
        lnks = foldl collectLinks mempty $ DV.toList $ getField @"body" s
        body = foldl addLinkRef
               (PP.indent 2 &! (const (getField @"body" s) <$> sl))
               $ L.reverse lnks
        addLinkRef a (l, mbd) =
          case mbd of
            Nothing -> a  -- already shown inline
            Just d -> a &< "" &< '[' &+ d &+ "] <" &+ l &+ '>'
        noBody = L.null
                 $ L.filter (not . isDrawer)
                 $ DV.toList
                 $ getField @"body" s
        noSubs = L.null $ DV.toList $ getField @"sections" s
    in hdr &< (if noBody
               then if noSubs
                    then blank
                    else "" &< subs
               else if noSubs
                    then "" &< body
                    else "" &< body &< "" &< subs
              )


----------------------------------------------------------------------

instance ($(sayableSubConstraints $ ofType ''OrgBody >> tagSym "text-style1")
         , Sayable "text-style1" OrgText
         , Sayable "text-style1" BodyList
         ) => Sayable "text-style1" OrgBody where
  sayable = sayable . slEmptyWrap

instance ( Sayable "text-style1" OrgBody
         ) => Sayable "text-style1" (SectionLevel OrgBody) where
  sayable sl =
    case slItem sl of
      OrgBody_paragraph p ->
        sayable @"text-style1" ((const $ orgMarkupParse p) <$> sl)
      OrgBody_dashList l -> PP.align &! (const (BulletList '•' l) <$> sl)
      OrgBody_plusList l -> PP.align &! (const (BulletList '⁃' l) <$> sl)
      OrgBody_enumList l -> PP.align &! (const (EnumList l) <$> sl)
      OrgBody_splatList l -> PP.align &! (const (BulletList '*' l) <$> sl)
      OrgBody_aBlock b -> sayable @"text-style1" b
      OrgBody_drawer {} -> blank
      OrgBody_setting setting ->
        case fmap toUpper $ DV.vecToString $ getField @"keyword" setting of
          "AUTHOR" -> centerLine 70
                      $ orgMarkupParseLine (getField @"values" setting)
          "EMAIL" -> centerLine 70
                     $ orgMarkupParseLine (getField @"values" setting)
          "TITLE" -> let ttl = orgMarkupParseLine $ getField @"values" setting
                         ttllen = max (displayLength ttl)
                                  $ case slSubtitle sl of
                                      Nothing -> 0
                                      Just st -> displayLength st
                     in centerLine 70 (replicate ttllen '━')
                        &< centerLine 70 ttl
                        &<? (centerLine 70 <$> slSubtitle sl)
                        &< centerLine 70 (replicate ttllen '━')
          _ -> blank

----------------------------------------------------------------------

instance Sayable "text-style1" (UInt 8) where
  sayable = sayable @"text-style1" . chr . fromInteger . asInt

instance Sayable "text-style1" (DV.Vector (UInt 8)) where
  sayable = go . DV.vecToString
    where
      go = \case
        w | "..." `L.isSuffixOf` w -> L.take (L.length w - 3) w &+ '…' &+ ' '
        w | "-" `L.isInfixOf` w ->
            let (x,y) = L.break (== '-') w
            in case y of
                 ('-':'-':'-':y') -> x &+ '—' &+ go y'
                 ('-':'-':y') -> x &+ '–' &+ go y'
                 ('-':y') -> x &+ '-' &+ go y'
                 _ -> x &+ y
        o -> sayable @"text-style1" o


instance Sayable "text-style1" (DV.Vector (DV.Vector (UInt 8))) where
  sayable = foldlNES @"text-style1" (&-) . DV.toList

instance Sayable "text-style1" (DV.Vector (DV.Vector (DV.Vector (UInt 8)))) where
  sayable = foldl (&<) (sayable @"text-style1" "r2") . DV.toList


----------------------------------------------------------------------

data BodyList = BulletList Char (DV.Vector ListItem)
              | EnumList (DV.Vector ListItem) -- KWQ: separator? . )

instance ( Sayable "text-style1" (DV.Vector ListItem)
         , Sayable "text-style1" ListItem
         ) => Sayable "text-style1" BodyList where
  sayable = sayable . slEmptyWrap

instance ( Sayable "text-style1" (SectionLevel (DV.Vector ListItem))
         , Sayable "text-style1" (SectionLevel ListItem)
         ) => Sayable "text-style1" (SectionLevel BodyList) where
  sayable sl =
    case slItem sl of
      (BulletList c lis) ->
        let eachItem i = c &- (PP.align &! (const i <$> sl))
        in foldlNES (listSep lis) (eachItem <$> DV.toList lis)
      (EnumList lis) ->
        let eachItem (c,i) = c &+ '.' &- (PP.align &! (const i <$> sl))
        in foldlNES (listSep lis) (eachItem <$> zip [(1::Int)..] (DV.toList lis))

listSep :: ( HasField "more" a (DV.Vector OrgBody)
           , HasField "entry" a OrgPara
           , DV.VecElem OrgBody
           , DV.VecElem a
           , Sayable saytag c
           ) => DV.Vector a -> Saying saytag -> c -> Saying saytag
listSep lis =
  let itemLengths = itemSize <$> DV.toList lis
      itemSize le =
        let entrySize = maximum
                        $ fmap displayLength
                        $ orgMarkupParse
                        $ getField @"entry" le
        in bool 999 entrySize $ null $ DV.toList $ getField @"more" le
  in bool withBlankLine (&<) $ maximum itemLengths < 70

instance Sayable "text-style1" ListItem => Sayable "text-style1" (DV.Vector ListItem) where
  sayable = sayable . slEmptyWrap

instance Sayable "text-style1" ListItem where
  sayable = sayable . slEmptyWrap

instance Sayable "text-style1" (SectionLevel ListItem)
  => Sayable "text-style1" (SectionLevel (DV.Vector ListItem)) where
  sayable sl =
    let eachItem s i = s &< '-' &- PP.align &! (const i <$> sl)
    in foldlNES eachItem $ DV.toList $ slItem sl

instance Sayable "text-style1" (SectionLevel ListItem) where
  sayable sl =
    let li = slItem sl
        cb = case chr . fromInteger . asInt <$> getField @"checkbox" li of
               Just ' ' -> '☐' &+ ' '
               Just '-' -> '☒' &+ ' '
               Just 'X' -> '☑' &+ ' '
               Just c -> '[' &+ c &+ "] "
               Nothing -> blank
        term = case getField @"term" li of
                 Just t -> PP.annotate #item &! orgMarkupParseLine t &- ":: "
                 Nothing -> blank
        entry = orgMarkupParse $ getField @"entry" li
        more = getField @"more" li
    in if L.null (DV.toList more)
       then cb &+ term &+ (const entry <$> sl)
       else cb &+ term &+ (const entry <$> sl) &< "" &< (const more <$> sl)

----------------------------------------------------------------------

instance Sayable "text-style1" Block where
  sayable blk =
    case DV.vecToString $ getField @"type" blk of
      code | (toUpper <$> code) == "SRC" ->
        let clines = foldlNES addLine (DV.toList $ getField @"contents" blk)
            addLine a b = a &< '│' &- b
        in "┌────" &< '│' &- clines &< "└────"
      code | (toUpper <$> code) == "EXAMPLE" ->
        let clines = foldlNES addLine (DV.toList $ getField @"contents" blk)
            addLine a b = a &< '│' &- b
        in "┌────" &< '│' &- clines &< "└────"
      code | (toUpper <$> code) == "QUOTE" ->
        PP.indent 6 &! foldlNES (&-) (stripLeadingWhitespace
                                      $ DV.toList $ getField @"contents" blk)
      code | (toUpper <$> code) == "VERSE" ->
        PP.indent 6 &! foldlNES (&<) (DV.toList $ getField @"contents" blk)
      code | (toUpper <$> code) == "CENTER" ->
        let eachLine l = centerLine 70 l
        in foldlNES (&<) (eachLine <$> DV.toList (getField @"contents" blk))
      _ ->
        foldlNES (&<) (DV.toList $ getField @"contents" blk)


instance Sayable "text-style1" Drawer where
  sayable = undefined

instance Sayable "text-style1" Setting where
  sayable = undefined

----------------------------------------------------------------------

instance ( Sayable "text-style1" OrgText
         ) => Sayable "text-style1" [OrgText] where
  sayable = sayable . slEmptyWrap

instance Sayable "text-style1" OrgText where
  sayable = sayable . slEmptyWrap

instance ( Sayable "text-style1" (SectionLevel OrgText)
         ) => Sayable "text-style1" (SectionLevel [OrgText]) where
  sayable sl =
      let ots = slItem sl
          moreSay ((s,ls), a) ot =
            let s' = case ot of
                       OrgText_adj _ -> True
                       _ -> False
            in ( (False, case ot of
                           OrgText_link {} -> ot : ls
                           _ -> ls)
               , if or [s, s']
                 then a &+ (const ot <$> sl)
                 else a &- (const ot <$> sl)
               )
          r = foldl moreSay ((True, []), sayable @"text-style1" "") ots
      in if L.null ots
         then "" &+ ""
         else snd r

instance Sayable "text-style1" (SectionLevel OrgText) where
  sayable sl =
    let ot = slItem sl
        toLines ts =
          let paras = L.groupBy (const (not . T.null)) $ concat ts
              eachPara ls = PP.fillSep (saying . sayable @"text-style1" <$> ls)
          in foldlNES @"text-style1" withBlankLine (eachPara <$> paras)
    in case ot of
         OrgText_text txt -> toLines txt
         OrgText_adj txt -> toLines txt
         OrgText_link lnk -> sayable @"text-style1" lnk
         OrgText_code tl -> '`' &+ PP.annotate #code &! toLines tl &+ '\''
         OrgText_bold e -> PP.annotate #bold &! ('*' &+ (const e <$> sl) &+ '*')
         OrgText_italics e -> PP.annotate #italics
                              &! ('/' &+ (const e <$> sl) &+ '/')
         OrgText_underline e -> PP.annotate #underline
                                &! ('_' &+ (const e <$> sl) &+ '_')
         OrgText_verbatim e -> PP.annotate #verbatim
                               &! ('`' &+ (const e <$> sl) &+ "'")
         OrgText_strikethrough e -> PP.annotate #strikethrough
                                    &! ('+' &+ (const e <$> sl) &+ '+')
         OrgText_link_target _ -> blank
         OrgText_radio_target t -> sayable @"text-style1" t -- target ignored for now
         OrgText_export kind ls -> bool blank (toLines ls) $ kind == T.pack "@ascii"

instance Sayable "text-style1" [OrgText' t] => Sayable "text-style1" (OrgLink t) where
  sayable (OrgLink l mbd) =
    case mbd of
      Just d -> '[' &+ sayable @"text-style1" d &+ ']'
      Nothing -> PP.annotate #link &! sayable @"text-style1" l
