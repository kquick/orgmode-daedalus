{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |
Module       : OrgMode.Sayable.Defs
Description  : Common definitions for pretty-printing of OrgMode documents
Copyright    : (c) 2026, Kevin Quick
License      : BSD3
Stability    : provisional
Portability  : portable

-}

module OrgMode.Sayable.Defs
  (
  -- internal use:
    LinkCollector(collectLinks)
  , foldlNES
  , blank
  , withBlankLine
  , stripLeadingWhitespace
  )
where

import           Daedalus.RTS
import qualified Daedalus.RTS.Vector as DV
import           Data.Char ( isSpace )
import Data.Functor.Foldable
import qualified Data.List as L
import qualified Data.Text as T
import           GHC.Records ( getField )
import           Text.Sayable

import           OrgMode.Markup
import           OrgMode.Parse


foldlNES :: forall saytag a .
            Sayable saytag a
         => (Saying saytag -> a -> Saying saytag) -> [a] -> Saying saytag
foldlNES op = \case
  [] -> sayable @saytag ""
  (a:as) -> foldl op (sayable @saytag a) as


blank :: Saying saytag
blank = "" &+ ""

withBlankLine :: Sayable saytag a => Saying saytag -> a -> Saying saytag
withBlankLine a b = a &< "" &< b

stripLeadingWhitespace :: [DV.Vector (UInt 8)] -> [DV.Vector (UInt 8)]
stripLeadingWhitespace = go . filter (not . all isSpace . DV.vecToString)
  where
    go = fmap stripWord
    stripWord = DV.fromList . L.dropWhile (== lit 32) . DV.toList

class LinkCollector a where
  collectLinks :: [(T.Text, Maybe [OrgText])]
               -> a
               -> [(T.Text, Maybe [OrgText])]

instance LinkCollector OrgBody where
  collectLinks ls = \case
    OrgBody_dashList l -> foldl collectLinks ls $ DV.toList l
    OrgBody_plusList l -> foldl collectLinks ls $ DV.toList l
    OrgBody_enumList l -> foldl collectLinks ls $ DV.toList l
    OrgBody_splatList l -> foldl collectLinks ls $ DV.toList l
    OrgBody_drawer d -> foldl collectLinks ls $ DV.toList
                        $ getField @"contents" d
    OrgBody_aBlock {} -> ls
    OrgBody_setting {} -> ls
    OrgBody_paragraph p -> foldl collectLinks ls $ orgMarkupParse p

instance LinkCollector ListItem where
  collectLinks s li =
    let el = foldl collectLinks s $ orgMarkupParse $ getField @"entry" li
    in foldl collectLinks el $ DV.toList $ getField @"more" li

instance LinkCollector OrgText where
  collectLinks ls = (<> ls) . cata go
    where
      go = \case
        OrgText_linkF lnk -> collectLinks mempty lnk
        OrgText_textF {} -> []
        OrgText_adjF {} -> []
        OrgText_codeF {} -> []
        OrgText_boldF od -> concat od
        OrgText_italicsF od -> concat od
        OrgText_underlineF od -> concat od
        OrgText_verbatimF od -> concat od
        OrgText_strikethroughF od -> concat od
        OrgText_link_targetF {} -> []
        OrgText_radio_targetF od -> concat od

instance LinkCollector (OrgLink T.Text) where
  collectLinks ls (OrgLink l d) = (l, d) : ls
