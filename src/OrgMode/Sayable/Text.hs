{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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
Module       : OrgMode.Sayable.Text
Description  : Pretty-printing of OrgMode documents to Text/UTF-8 output
Copyright    : (c) 2026, Kevin Quick
License      : BSD3
Stability    : provisional
Portability  : portable

Sayable "text" instances for org-mode ASTs, as parsed by Daedalus and
Data.OrgMode.Parse.orgParse.  This is roughly equivalent to emacs' ox-text output
emitter.
-}

module OrgMode.Sayable.Text
  (
    -- exports many instances of Sayable
  )
where

import           Data.Bool ( bool )
import           Data.Char ( chr, toUpper )
import qualified Data.List as L
import qualified Data.Text as T
import           GHC.Records ( getField )
import qualified Prettyprinter as PP
import           Text.Sayable

import           Daedalus.RTS hiding ( div )
import qualified Daedalus.RTS.Vector as DV
import           OrgMode.Markup
import           OrgMode.Parse
import           OrgMode.Sayable.Defs


-- The general implementation guideline here is that lists/vectors of items
-- intersperse a blank line between their items, but overall elements do not add
-- leading or trailing space.


instance ( $(sayableSubConstraints $ ofType ''OrgDoc >> tagSym "text")
         ) => Sayable "text" OrgDoc where
  sayable od =
    -- show od &<
    if null $ DV.toList $ getField @"body" od
    then sayable @"text" $ getField @"sections" od &< ""
    else if null $ DV.toList $ getField @"sections" od
         then sayable @"text" $ getField @"body" od &< ""
         else getField @"body" od &< "" &< getField @"sections" od


----------------------------------------------------------------------

instance ( Sayable "text" OrgBody
         ) => Sayable "text" (DV.Vector OrgBody) where
  sayable =
    let joinLines a b =
          case b of
            OrgBody_setting setting ->
              -- n.b. since some settings are not applied, break the newline
              -- general rule and be explicit here when appropriate.
              let kw = fmap toUpper $ DV.vecToString $ getField @"keyword" setting
              in if kw `elem` [ "TITLE", "SUBTITLE", "AUTHOR", "EMAIL" ]
                 then a &< b
                 else a  -- drop b, it's probably blank (see Sayable OrgBody)
            _ -> withBlankLine a b
    in foldlNES @"text" joinLines . filter (not . isDrawer) . DV.toList


----------------------------------------------------------------------

instance ( $(sayableSubConstraints $ ofType ''OrgSection >> tagSym "text")
         , Sayable "text" OrgSection
         ) => Sayable "text" (DV.Vector OrgSection) where
  sayable = foldlNES @"text" withBlankLine . DV.toList

instance
  ( $(sayableSubConstraints $ ofType ''OrgSection >> tagSym "text")
  ) => Sayable "text" OrgSection where
  sayable s = let hdr = orgMarkupParseLine $ getField @"header" s
                  len = fromInteger $ asInt $ getField @"level" s
                  bdy = getField @"body" s
                  tdo = addTodoAnn . DV.vecToString <$> getField @"todo" s
                  addTodoAnn "TODO" = PP.annotate #todo &! "TODO"
                  addTodoAnn "DONE" = PP.annotate #done &! "DONE"
                  addTodoAnn o = PP.annotate #todo &! o
                  sline = T.replicate len (T.singleton '*') &? tdo &- hdr
              in sline &< (if L.null $ L.filter (not . isDrawer) $ DV.toList bdy
                           then if L.null $ DV.toList $ getField @"sections" s
                                then blank
                                else "" &< getField @"sections" s
                           else ""
                                &< PP.indent 2 &! bdy
                                &< ""
                                &< getField @"sections" s
                          )


----------------------------------------------------------------------

instance ($(sayableSubConstraints $ ofType ''OrgBody >> tagSym "text")
         , Sayable "text" OrgText
         , Sayable "text" BodyList
         ) => Sayable "text" OrgBody where
  sayable = \case
    OrgBody_dashList l -> (PP.align &! BulletList '-' l)
    OrgBody_plusList l -> (PP.align &! BulletList '+' l)
    OrgBody_enumList l -> (PP.align &! EnumList l)
    OrgBody_splatList l -> (PP.align &! BulletList '*' l)
    OrgBody_drawer {} -> blank
    OrgBody_aBlock bls -> PP.indent 4 &! bls
    OrgBody_setting setting ->
      case fmap toUpper $ DV.vecToString $ getField @"keyword" setting of
        "AUTHOR" -> "  by" &- orgMarkupParseLine (getField @"values" setting)
        "EMAIL" -> "    " &- orgMarkupParseLine (getField @"values" setting)
        "TITLE" -> let ttl = orgMarkupParseLine $ getField @"values" setting
                       ttllen = displayLength ttl
                   in ttl &< replicate ttllen '='
        "SUBTITLE" -> let ttl = orgMarkupParseLine $ getField @"values" setting
                          ttllen = displayLength ttl
                      in ttl &< replicate ttllen '-'
        _ -> blank
    OrgBody_paragraph p -> sayable @"text" $ orgMarkupParse p


----------------------------------------------------------------------

instance Sayable "text" (UInt 8) where
  sayable = sayable @"text" . chr . fromInteger . asInt

instance Sayable "text" (DV.Vector (UInt 8)) where
  sayable = sayable @"text" . DV.vecToString

instance Sayable "text" (DV.Vector (DV.Vector (UInt 8))) where
  sayable = foldlNES @"text" (&-) . DV.toList

instance Sayable "text" (DV.Vector (DV.Vector (DV.Vector (UInt 8)))) where
  sayable = foldl (&<) (sayable @"text" "r2") . DV.toList

----------------------------------------------------------------------

data BodyList = BulletList Char (DV.Vector ListItem)
              | EnumList (DV.Vector ListItem) -- KWQ: separator? . )

instance ( Sayable "text" (DV.Vector ListItem)
         , Sayable "text" ListItem
         ) => Sayable "text" BodyList where
  sayable = \case
    BulletList c lis ->
      let eachItem i = c &- PP.align &! i
      in foldlNES withBlankLine (eachItem <$> DV.toList lis)
    EnumList lis ->
      let eachItem (c,i) = c &+ '.' &- PP.align &! i
      in foldlNES withBlankLine (eachItem <$> zip [(1::Int)..] (DV.toList lis))


instance Sayable "text" ListItem => Sayable "text" (DV.Vector ListItem) where
  sayable = let eachItem s i = s &< '-' &- PP.align &! i
            in foldlNES eachItem . DV.toList

instance Sayable "text" ListItem where
  sayable li =
    let cb = case getField @"checkbox" li of
               Just c -> '[' &+ c &+ "] "
               Nothing -> blank
        term = case getField @"term" li of
                 Just t -> PP.annotate #item &! orgMarkupParseLine t &- ":: "
                 Nothing -> blank
        entry = orgMarkupParse $ getField @"entry" li
        more = getField @"more" li
    in if (L.null $ DV.toList more)
       then cb &+ term &+ entry
       else cb &+ term &+ entry &< "" &< more


----------------------------------------------------------------------

instance Sayable "text" Block where
  sayable blk =
    case DV.vecToString $ getField @"type" blk of
      code | (toUpper <$> code) == "SRC" ->
        let lang = case DV.toList $ getField @"args" blk of
                     (l:_) -> DV.vecToString l
                     [] -> ""
            clines = foldlNES (&<) (DV.toList $ getField @"contents" blk)
        in '[' &+ lang &+ ']' &< clines
      code | (toUpper <$> code) == "EXAMPLE" ->
        foldlNES (&<) (DV.toList $ getField @"contents" blk)
      code | (toUpper <$> code) == "QUOTE" ->
        '"' &+ PP.align &! foldlNES (&<) (stripLeadingWhitespace
                                          $ DV.toList
                                          $ getField @"contents" blk)
        &+ '"'
      code | (toUpper <$> code) == "VERSE" ->
        PP.indent 2 &! foldlNES (&<) (DV.toList $ getField @"contents" blk)
      code | (toUpper <$> code) == "CENTER" ->
        let eachLine l = let l' = sez @"text" l
                             i = max 0 (35 - (length l' `div` 2 + 1))
                         in L.replicate i ' ' &+ l
        in foldlNES (&<) (eachLine <$> DV.toList (getField @"contents" blk))
      _ ->
        foldlNES (&<) (DV.toList $ getField @"contents" blk)


instance Sayable "text" Drawer where
  sayable = undefined

instance Sayable "text" Setting where
  sayable = undefined

----------------------------------------------------------------------

instance ( Sayable "text" OrgText
         ) => Sayable "text" [OrgText] where
  sayable ots =
    let moreSay (s, a) ot =
          let s' = case ot of
                     OrgText_adj _ -> True
                     _ -> False
          in (False, if or [s, s'] then a &+ ot else a &- ot)
    in if L.null ots
       then blank
       else snd (foldl moreSay (True, sayable @"text" "") ots)

instance Sayable "text" OrgText where
  sayable =
    let toLines ts =
            let paras = L.groupBy (const (not . T.null)) $ concat ts
                eachPara ls = PP.fillSep (saying . sayable @"text" <$> ls)
            in foldlNES @"text" withBlankLine (eachPara <$> paras)
    in \case
      OrgText_text txt -> toLines txt
      OrgText_adj txt -> toLines txt
      OrgText_link lnk -> sayable lnk
      OrgText_code tl -> '`' &+ PP.annotate #code &! toLines tl &+ '\''
      OrgText_bold e -> PP.annotate #bold &! e
      OrgText_italics e -> PP.annotate #italics &! e
      OrgText_underline e -> PP.annotate #underline &! e
      OrgText_verbatim e -> PP.annotate #verbatim &! e
      OrgText_strikethrough _ -> blank -- don't show this
      OrgText_link_target _ -> blank -- nothing to show
      OrgText_radio_target t -> sayable t -- target ignored for now
      OrgText_export kind ls -> bool blank (toLines ls) $ kind == T.pack "@ascii"

instance Sayable "text" [OrgText' t] => Sayable "text" (OrgLink t) where
  sayable (OrgLink l mbd) =
    case mbd of
      Just d -> sayable @"text" d &- '(' &+ PP.annotate #link &! l &+ ')'
      Nothing -> PP.annotate #link &! sayable @"text" l
