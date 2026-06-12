{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module OrgMode.Markup
  (
    orgMarkupParse
  , orgMarkupParseLine
  , OrgText'(..), OrgText
  , OrgPara
  , displayLength
  , displayLength'
  )
where

import           Daedalus.RTS
import qualified Daedalus.RTS.Vector as DV
import           Data.Bool ( bool )
import qualified Data.Foldable as F
import qualified Data.List as L
import           Data.Maybe ( catMaybes, isJust )
import           Data.Text ( Text )
import qualified Data.Text as T
import           Panic


-- | This parses the markup in an OrgBody_para into OrgText.
--
-- Para = (lines = [line] = [[word]] = [[[char-as-uint8]]]
--
-- out: OrgText = OrgText_text [[[uint 8]]]
--              | OrgText_bold [OrgText]
--              | OrgText_italics [OrgText]
--              | OrgText_underline [OrgText]
--
-- Note that each OrgText_text specifies a number of lines, but the boundaries
-- between OrgText are *not* a line-break.
--
-- For example:
--
--      Hello, world
--      Hiya *to _you_* /back/!
--
-- is parsed as:
--
--   [ OrgText_text [["Hello,", "world"], ["Hiya"]]
--   , OrgText_bold [ OrgText_text [["to"]]
--                  , OrgText_underline [OrgText [["you"]]]
--                  ]
--   , OrgText_italics [ OrgText_text [["back"]] ]
--   , OrgText_text [["!"]]
--   ]
--
-- Markup rules:
--
--  1. Markup is started with a markup character at the beginning of a word and
--     is stopped by the same markup character at the end of a word (excluding
--     puntuation, except links which cannot have following punctuation).
--
--  2. The word (or each word) must have at least one character, exclusive of
--     markup characters.
--
--  3. Markup may span multiple lines
--
--  4. Markup start without markup stop means it was just a regular character and
--     not a markup start.
--
--  5. Markup may be nested
--     a. Closure of outer-markup before inner markup means inner was not a
--        start markup character and was just a regular character
orgMarkupParse :: OrgPara -> [OrgText]
orgMarkupParse = detectMarkup . joined . toLists
  where
    toLists :: OrgPara -> [[Text]]
    toLists = fmap (fmap (T.pack . DV.vecToString) . DV.toList) . DV.toList

    joined :: [[Text]] -> [Maybe Text]
    joined = concat . fmap (reverse . (Nothing :) . reverse . fmap Just)


-- | This parses the markup in an Line into OrgText.  This is the version of
-- orgMarkupParse that acts on a single line (e.g. a Section Header).
--
-- line = [word] = [[char-as-uint8]]
--
-- out: OrgText = OrgText_text [[[uint 8]]]
--              | OrgText_bold [OrgText]
--              | OrgText_italics [OrgText]
--              | OrgText_underline [OrgText]
--
-- Note that each OrgText_text specifies a number of lines, but the boundaries
-- between OrgText are *not* a line-break.
orgMarkupParseLine :: DV.Vector (DV.Vector (UInt 8)) -> [OrgText]
orgMarkupParseLine = detectMarkup . toMaybeList
  where
    toMaybeList = fmap (Just . T.pack . DV.vecToString) . DV.toList


detectMarkup :: [Maybe Text] -> [OrgText]
detectMarkup =
      L.reverse
      . (\((_m,(i,p)), g) ->
           -- m does not matter: did not see stop markup mark, so this gets
           -- rendered as plain
           fst $ newGroupWithMarkup NoMarkup p i g)
      . foldl groupByMarkup groupByMarkupSeed

  where
    backToLines :: [Maybe Text] -> [[Text]]
    backToLines = fmap catMaybes . L.groupBy (const isJust)

    groupByMarkupSeed :: ((MarkupContext, ([Maybe Text], Bool)), [OrgText])
    groupByMarkupSeed = ((NoMarkup,   -- what markup is active now
                          (mempty,    -- text accumulated under this markup
                           False)),   -- is this adj text?
                          [])         -- accumulated output

    groupByMarkup :: ((MarkupContext, ([Maybe Text], Bool)), [OrgText])
                  -> Maybe Text
                  -> ((MarkupContext, ([Maybe Text], Bool)), [OrgText])
    groupByMarkup ((curMark, (inProg, isAdj)), groups) w =
      case curMark of
        NoMarkup ->
          case begMarkup w of
            Nothing ->
              case w of
                Just wt | "[[" `T.isPrefixOf` wt ->
                  let wtp = T.dropEnd 2 $ T.drop 2 wt
                      (acc, (newAcc, newAdj))
                        = newGroupWithMarkup curMark isAdj inProg groups
                  in if "]]" `T.isSuffixOf` wt
                     then
                       case L.uncons $ T.splitOn "][" wt of
                         Just (lw,d) | not (L.null d) ->
                           -- lw is Text, d is [Text], |d| is 1
                           ((NoMarkup, (newAcc, newAdj))
                           , OrgText_link
                             (T.drop 2 lw)
                             (Just (detectMarkup (Just . T.dropEnd 2 <$> d)))
                             : acc)
                         _ ->
                           ((NoMarkup, (newAcc, newAdj))
                           , OrgText_link wtp Nothing : acc)
                     else
                           ((Link, (addToCurr w newAcc, newAdj)), acc)
                _ ->
                  -- same plain group, add to current group
                  ((curMark, (addToCurr w inProg, isAdj)), groups)
            Just m ->
              -- one-word markup?
              -- yes: close inprog and add one-word markup
              -- no: start a new group with this markup char
              if endMarkupMatches m w
              then let (acc, (newInProg', isAdj'')) =
                         newGroupWithMarkup NoMarkup isAdj inProg groups
                       (acc2, newInProgAdj) =
                         newGroupWithMarkup (Markup m) isAdj'' (w:newInProg')
                         acc
                   in ((NoMarkup, newInProgAdj), acc2)
              else let (acc, (newInProg, isAdj')) =
                         newGroupWithMarkup curMark isAdj inProg groups
                   in ((Markup m, (addToCurr w newInProg, isAdj')), acc)
        Markup m ->
          if endMarkupMatches m w
          then let (acc, newInProgAdj) =
                     newGroupWithMarkup curMark isAdj (addToCurr w inProg) groups
               in ((NoMarkup, newInProgAdj), acc)
          else -- same markup group, add to current group
            ((curMark, (addToCurr w inProg, isAdj)), groups)
        Link ->
          case ("]]" `T.isSuffixOf`) <$> w of
            Just True ->
              let (acc, newInProgAdj) =
                    newGroupWithMarkup curMark isAdj
                    (addToCurr (T.dropEnd 2 <$> w) inProg) groups
              in ((NoMarkup, newInProgAdj), acc)
            _ -> -- same markup group, add to current group
              ((curMark, (addToCurr w inProg, isAdj)), groups)

    newGroupWithMarkup :: MarkupContext -> Bool -> [Maybe Text] -> [OrgText]
                       -> ([OrgText], ([Maybe Text], Bool))
    newGroupWithMarkup m isAdj t acc =
      let t' = mkOrgText t
          mkOrgText = filter (not . null) . backToLines . L.reverse
          orgTxtC = bool OrgText_text OrgText_adj isAdj
          (t0,p) = removeEndMarkup m t
          t1 = L.reverse $ removeBegMarkup m t0
          t1m = detectMarkup t1
          newAccAdj = case p of
                        Nothing -> ([], False)
                        Just _ -> ([p], True)
          joinPlain = \case
            (OrgText_text a : OrgText_text b : c) ->
              case L.uncons a of
                Just (a', as) ->
                  case L.uncons $ L.reverse b of
                    Just (b', bs) ->
                      OrgText_text (L.reverse bs <> [b' <> a'] <> as) : c
                    Nothing ->
                      OrgText_text a : c
                Nothing -> OrgText_text b : c
            o -> o
      in case m of
           NoMarkup -> if null t'
                       then (acc, ([], False))
                       else (joinPlain $ orgTxtC t' : acc, ([], False))
           Markup c ->
             if null t1
             then (acc, ([], isAdj))
             else ((if c == "~" -- separate because inner is literal
                    then OrgText_code $ backToLines t1
                    else
                       case c of
                         "*" -> OrgText_bold t1m
                         "_" -> OrgText_underline t1m
                         "/" -> OrgText_italics t1m
                         ">>" -> OrgText_link_target $ T.concat $ catMaybes t1
                         ">>>" -> OrgText_radio_target t1m
                         o -> panic OrgMarkup "markupConstr"
                              ["Unknown/unsupported markup marker: " <> show o]
                   ) : acc
                  , newAccAdj)
           Link ->
             let mkLink l d = OrgText_link (T.drop 2 l) d : acc
             in case L.uncons $ L.reverse t of
                  Just (Just w1, ws) ->
                    case L.uncons $ T.splitOn "][" w1 of
                      Just (lw,d1) ->
                        (mkLink lw (Just $ detectMarkup $ (Just <$> d1) <> ws)
                        , (mempty, False))
                      Nothing -> (mkLink w1 Nothing, (mempty, False))
                  Just (Nothing, _) ->
                    panic OrgMarkup "groupByMarkup Link Just Nothing"
                    [ "Link markup cannot start with newline"]
                  Nothing ->
                    -- empty link, so this is actually just plain text
                    (acc, ([Just "[[]]"], False))

    addToCurr :: Maybe Text -> [Maybe Text] -> [Maybe Text]
    addToCurr = (:)

    -- n.b. returns what should be the *end* markup if this is the beginning
    begMarkup :: Maybe Text -> Maybe Text
    begMarkup = maybe Nothing $ \w ->
      case T.uncons w of
        Just (c,w') | c `elem` markupChars, not (T.null w') ->
          Just $ T.singleton c
        Just ('<',w') | not (T.null w') ->
          case T.uncons w' of
            Just ('<',w'') | not (T.null w'') ->
              case T.uncons w'' of
                Just (c',w3) ->
                  if c' == '<' && not (T.null w3) then Just ">>>" else Just ">>"
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing

    -- avoid ending punctuation
    endMarkupMatches :: Text -> Maybe Text -> Bool
    endMarkupMatches e = maybe False $ \w ->
      let w' = T.dropWhileEnd (`elem` endPunctuation) w
      in e `T.isSuffixOf` w'

    -- n.b. the passed markup is actually the *end* markup.  At present, the
    -- length of the beginning markup and end markup match, so this just uses the
    -- char count.
    removeBegMarkup :: MarkupContext -> [Maybe Text] -> [Maybe Text]
    removeBegMarkup m ws =
      let rmv = case m of
            Markup m' -> T.drop (T.length m')
            _ -> panic OrgMarkup "removeBegMarkup rmv"
                 [ "Should not be called with other types of markup" ]
      in case L.reverse ws of
           (Just w:ws') ->
             L.reverse $ Just (rmv w) : ws'
           [] -> panic OrgMarkup "removeBegMarkup - empty"
                 [ "Should not have an empty list for a markup region start." ]
           (Nothing:_) ->
             panic OrgMarkup "removeBegMarkup - Nothing:"
             [ "First element should have been the markup opener." ]

    -- Avoid ending punctuation.
    removeEndMarkup :: MarkupContext -> [Maybe Text] -> ([Maybe Text], Maybe Text)
    removeEndMarkup m ws =
      let rmv = case m of
            Markup m' -> T.dropEnd (T.length m')
            _ -> panic OrgMarkup "removeBegMarkup rmv"
                 [ "Should not be called with other types of markup" ]
      in case L.uncons ws of
           Just (Just w,ws') ->
             let adj_punct = T.takeWhileEnd (`elem` endPunctuation) w
                 unpunct = T.dropWhileEnd (`elem` endPunctuation)
                 adj = if T.null adj_punct then Nothing else Just adj_punct
             in (Just (rmv $ unpunct w) : ws', adj)
           Nothing -> panic OrgMarkup "removeEndMarkup"
                      ["Should not have an empty list for a markup region end."]
           Just (Nothing, _) ->
             panic OrgMarkup "removeEndMarkup - Just Nothing"
             [ "Last element should have been the markup closure." ]

    markupChars = "*_/~" :: String
    endPunctuation = "!,.;'\")]:" :: String

data MarkupContext = NoMarkup | Markup Text | Link

-- | Type alias for an OrgBody_para
type OrgPara = DV.Vector (DV.Vector (DV.Vector (UInt 8)))

-- | Markup-carrying representation of a paragraph.
data OrgText' t = OrgText_text [[t]]
                | OrgText_adj [[t]]
                  -- ^ same as OrgText_text, except the last word of a previous
                  -- OrgText_text and the first word of the following OrgText_text
                  -- are separate words and should have whitespace betwen them,
                  -- whereas an OrgText_adj is immediately adjacent to the last
                  -- word of the previous OrgText_text (e.g. typically contains
                  -- punctuation characters).
                | OrgText_code [[Text]]
                | OrgText_bold [OrgText' t]
                | OrgText_italics [OrgText' t]
                | OrgText_underline [OrgText' t]
                | OrgText_link Text (Maybe [OrgText' t])
                  -- ^ the link and optional description
                | OrgText_link_target Text
                | OrgText_radio_target [OrgText' t]
  deriving (Eq, Show, Functor)

type OrgText = OrgText' Text

displayLength :: OrgText -> Int
displayLength =
  let maxLineLength = let wlen wrds = sum (fmap T.length wrds) + length wrds - 1
                      in maximum . fmap wlen
  in \case
    OrgText_text txtLines -> maxLineLength txtLines
    OrgText_adj txtLines -> maxLineLength txtLines
    OrgText_code codeLines -> maxLineLength codeLines
    OrgText_bold ot -> maximum (displayLength <$> ot)
    OrgText_italics ot -> maximum (displayLength <$> ot)
    OrgText_underline ot -> maximum (displayLength <$> ot)
    OrgText_link l mbd -> maybe (T.length l) (maximum . fmap displayLength) mbd
    OrgText_link_target _ -> 0
    OrgText_radio_target ot -> maximum (displayLength <$> ot)

displayLength' :: Foldable t => OrgText' (t a) -> Int
displayLength' =
  let maxLineLength = let wlen wrds = sum (fmap F.length wrds) + length wrds - 1
                      in maximum . fmap wlen
  in \case
    OrgText_text txtLines -> maxLineLength txtLines
    OrgText_adj txtLines -> maxLineLength txtLines
    OrgText_code codeLines ->
      let wlen wrds = sum (fmap T.length wrds) + length wrds - 1
      in maximum $ fmap wlen codeLines
    OrgText_bold ot -> maximum (displayLength' <$> ot)
    OrgText_italics ot -> maximum (displayLength' <$> ot)
    OrgText_underline ot -> maximum (displayLength' <$> ot)
    OrgText_link l mbd -> maybe (T.length l) (maximum . fmap displayLength') mbd
    OrgText_link_target _ -> 0
    OrgText_radio_target ot -> maximum (displayLength' <$> ot)


----------------------------------------------------------------------

data OrgMarkup = OrgMarkup

instance PanicComponent OrgMarkup where
  panicComponentName _ = "orgMarkupParse"
  panicComponentIssues _ = "kq1quick@gmail.com"
  panicComponentRevision _ = ("TBD", "")
