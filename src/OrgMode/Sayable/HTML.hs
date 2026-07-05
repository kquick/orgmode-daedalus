{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module       : OrgMode.Sayable.HTML
Description  : Pretty-printing of OrgMode documents to HTML
Copyright    : (c) 2026, Kevin Quick
License      : BSD3
Stability    : provisional
Portability  : portable

Sayable "html" instances for org-mode ASTs, as parsed by Daedalus and
Data.OrgMode.Parse.orgParse.  This is roughly equivalent to emacs' ox-html output
emitter.
-}

module OrgMode.Sayable.HTML
  (
    -- exports many instances of Sayable
  )
where

import           Data.Bool ( bool )
import           Data.Char ( chr, toUpper )
import           Data.Foldable
import qualified Data.List as L
import           Data.Maybe ( isJust )
import           Data.Text ( pack )
import           GHC.Records ( getField )
import           Text.Sayable

import           Daedalus.RTS
import qualified Daedalus.RTS.Vector as DV
import           OrgMode.Markup
import           OrgMode.Parse
import           OrgMode.Sayable.Defs

import           Prelude hiding ( lines )


instance ( $(sayableSubConstraints $ ofType ''OrgDoc >> tagSym "html")
         ) => Sayable "html" OrgDoc where
  sayable od =
    let get f = \case
          OrgBody_setting s
            | f == (toUpper <$> (DV.vecToString $ getField @"keyword" s))
            -> Just $ orgMarkupParseLine $ getField @"values" s
          _ -> Nothing
        subtitle = asum (get "SUBTITLE" <$> (DV.toList $ getField @"body" od))
        title = asum (get "TITLE" <$> (DV.toList $ getField @"body" od))
    in
      "<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">"
      &< "<head>"
      &< (DV.fromList $ L.filter isSetting $ DV.toList $ getField @"body" od)
      &< "</head>"
      &< "<body>"
      &<? (("<h1 class=\"title\">" &+) . (&+ "</h1>") <$> title)
      &<? (("<br/><span class=\"subtitle\">" &+) . (&+ "</span>") <$> subtitle)
      -- show od &<
      &< (DV.fromList $ L.filter (not . isSetting) $ DV.toList $ getField @"body" od)
      &< getField @"sections" od
      &< "</body>"
      &< "</html>"
      &< ""


----------------------------------------------------------------------

instance ( Sayable "html" OrgBody
         ) => Sayable "html" (DV.Vector OrgBody) where
  sayable =
    let joinLines a b =
          case b of
            OrgBody_setting setting ->
              -- n.b. since some settings are not applied, break the newline
              -- general rule and be explicit here when appropriate.
              let kw = fmap toUpper $ DV.vecToString $ getField @"keyword" setting
              in if kw `elem` [ "TITLE", "AUTHOR", "EMAIL"
                              , "DESCRIPTION"
                              , "HTML_HEAD"
                              , "HTML_HEAD_EXTRA"
                              , "HTML_DOCTYPE"
                              , "HTML_CONTAINER"
                              , "HTML_LINK_HOME"
                              , "HTML_LINK_UP"
                              , "HTML_MATHJAX"
                              , "KEYWORDS"
                              , "LATEX_HEADER"
                              , "SUBTITLE"
                              ]
                 then a &< b
                 else a  -- drop b, it's probably blank (see Sayable OrgBody)
            _ -> a &< b
    in foldlNES @"html" joinLines . DV.toList


----------------------------------------------------------------------

instance ( $(sayableSubConstraints $ ofType ''OrgSection >> tagSym "html")
         , Sayable "html" OrgSection
         ) => Sayable "html" (DV.Vector OrgSection) where
  sayable = foldlNES @"html" (&<) . DV.toList


instance ( $(sayableSubConstraints $ do ofType ''OrgSection
                                        tagSym "html")
         ) => Sayable "html" OrgSection where
  sayable s = let hdr = orgMarkupParseLine $ getField @"header" s
                  lvl = asInt $ getField @"level" s
                  tdo = (DV.vecToString <$> getField @"todo" s) >>= \case
                          "TODO" -> Just $ hspan "TODO TODO-TODO" "TODO" &+ ' '
                          "DONE" -> Just $ hspan "TODO TODO-DONE" "DONE" &+ ' '
                          o -> Just $ hspan "TODO TODO-todo" o &+ ' '
                  sline = "<h" &+ lvl &+ '>'
                          &+? tdo &+ hdr
                          &+ "</h" &+ lvl &+ '>'
              in sline
                 &< getField @"body" s
                 &< foldlNES @"html" (&<) (DV.toList $ getField @"sections" s)


----------------------------------------------------------------------

instance ($(sayableSubConstraints $ ofType ''OrgBody >> tagSym "html")
         , Sayable "html" OrgText
         ) => Sayable "html" OrgBody where
  sayable =
    let sayList l = if isDescriptionList l
                    then "<dl>" &< (DL <$> (DV.toList l)) &< "</dl>"
                    else "<ul>" &< l &< "</ul>"
    in \case
      OrgBody_dashList l -> sayList l
      OrgBody_plusList l -> sayList l
      OrgBody_enumList l -> "<ol>" &< l &< "</ol>"
      OrgBody_splatList l -> sayList l
      OrgBody_drawer {} -> blank
      OrgBody_aBlock b -> sayable @"html" b
      OrgBody_setting setting ->
        let vs = orgMarkupParseLine $ getField @"values" setting
        in case fmap toUpper $ DV.vecToString $ getField @"keyword" setting of
             "AUTHOR" -> "<meta name=\"author\" content=\"" &+ vs &+ '"' &- "/>"
             "DESCRIPTION" ->
               "<meta name=\"description\" content=\"" &+ vs &+ '"' &- "/>"
             "TITLE" -> "<title>" &+ vs &+ "</title>"
             "HTML_HEAD" -> sayable @"html" vs
             "HTML_HEAD_EXTRA" -> sayable @"html" vs
             _ -> blank
      OrgBody_paragraph p -> "<p>" &< sayable (orgMarkupParse p) &< "</p>"

isDescriptionList :: DV.Vector ListItem -> Bool
isDescriptionList = go . DV.toList
  where
    go [] = False
    go (li:lis) = (isJust $ getField @"term" li) || (go lis)

data DL = DL ListItem

----------------------------------------------------------------------

instance Sayable "html" (UInt 8) where
  sayable = sayable @"html" . chr . fromInteger . asInt

instance Sayable "html" (DV.Vector (UInt 8)) where
  sayable = go . DV.vecToString
    where
      go = \case
        w | "..." `L.isSuffixOf` w -> L.take (L.length w - 3) w &+ "&hellip;"
        w | "-" `L.isInfixOf` w ->
            let (x,y) = L.break (== '-') w
            in case y of
                 ('-':'-':'-':y') -> x &+ "&mdash;" &+ go y'
                 -- ('-':'-':y') -> x &+ '–' &+ go y'
                 -- ('-':y') -> x &+ '-' &+ go y'
                 _ -> x &+ y
        o -> sayable @"html" o

instance Sayable "html" (DV.Vector (DV.Vector (UInt 8))) where
  sayable = foldlNES @"html" (&-) . DV.toList

instance Sayable "html" (DV.Vector (DV.Vector (DV.Vector (UInt 8)))) where
  sayable = foldl (&<) (sayable @"html" "r2") . DV.toList

----------------------------------------------------------------------

instance Sayable "html" (DV.Vector ListItem) where
  sayable = foldlNES @"html" (&<) . DV.toList

instance Sayable "html" [ListItem] where
  sayable = foldlNES @"html" (&<)

instance Sayable "html" ListItem where
  sayable li = "<li>"
               &+ case getField @"checkbox" li of
                    Nothing -> blank
                    Just c ->
                      let cl = case chr $ fromInteger $ asInt c of
                            ' ' -> "Checkbox Checkbox-Empty"
                            '-' -> "Checkbox Checkbox-InProg"
                            'X' -> "Checkbox Checkbox-Marked"
                            _ -> "Checkbox"
                      in hspan cl ('[' &+ c &+ ']') &+ ' '
               &+ foldl (&<)
                  (sayable @"html" $ orgMarkupParse $ getField @"entry" li)
                  (sayable @"html" <$> DV.toList (getField @"more" li))
               &+ "</li>"

instance Sayable "html" [DL] where
  sayable = foldlNES @"html" (&<)

instance Sayable "html" DL where
  sayable (DL li) = "<dt>"
               &+ case getField @"checkbox" li of
                    Nothing -> blank
                    Just c -> '[' &+ c &+ "] "
               &+ case getField @"term" li of
                    Nothing -> '-' &+ '-'
                    Just t -> sayable @"html" $ orgMarkupParseLine t
               &+ "</dt>"
               &< "<dd>"
               &+ foldl (&<)
                  (sayable @"html" $ orgMarkupParse $ getField @"entry" li)
                  (sayable @"html" <$> DV.toList (getField @"more" li))
               &+ "</dd>"

----------------------------------------------------------------------

instance Sayable "html" Block where
  sayable blk =
    case DV.vecToString $ getField @"type" blk of
      code | (toUpper <$> code) == "SRC" ->
        let lang = case DV.toList $ getField @"args" blk of
                     (l:_) -> DV.vecToString l
                     [] -> ""
            clines = foldlNES (&<) (DV.toList $ getField @"contents" blk)
        in "<pre class=\"src src-" &+ lang &+ "\"><code>" &< clines &< "</code></pre>"
      code | (toUpper <$> code) == "EXAMPLE" ->
        "<pre class=\"example\">"
        &< foldlNES (&<) (DV.toList $ getField @"contents" blk)
        &< "</pre>"
      code | (toUpper <$> code) == "QUOTE" ->
        "<blockquote>"
        &< "<p>"
        &< foldlNES (&<) (DV.toList $ getField @"contents" blk)
        &< "</p>"
        &< "</blockquote>"
      code | (toUpper <$> code) == "VERSE" ->
        "<div class=\"verse\">"
        &< foldlNES (&<) ((&+ "<br/>") <$> DV.toList (getField @"contents" blk))
        &< "</div>"
      code | (toUpper <$> code) == "CENTER" ->
        "<div class=\"center\">"
        &< foldlNES (&<) (DV.toList (getField @"contents" blk))
        &< "</div>"
      _ ->
        foldlNES (&<) (DV.toList $ getField @"contents" blk)


----------------------------------------------------------------------

instance Sayable "html" Drawer where
  sayable = undefined

----------------------------------------------------------------------

instance Sayable "html" Setting where
  sayable = undefined

----------------------------------------------------------------------

instance ( Sayable "html" OrgText
         ) => Sayable "html" [OrgText] where
  sayable [] = blank
  sayable ots =
    let moreSay (s, a) ot =
          let s' = case ot of
                     OrgText_adj _ -> True
                     _ -> False
          in (False, if or [s, s'] then a &+ ot else a &- ot)
    in snd $ foldl moreSay (True, sayable @"html" "") ots

instance Sayable "html" OrgText where
  sayable =
    let toLines = foldlNES (&<) . fmap toLine
        toLine = foldlNES (&-)
    in \case
      OrgText_text txt -> toLines txt
      OrgText_adj txt -> toLines txt
      OrgText_link lnk -> sayable @"html" lnk
      OrgText_code ls -> hspan "code" (toLines ls)
      OrgText_bold e -> "<b>" &+ e &+ "</b>"
      OrgText_italics e -> "<i>" &+ e &+ "</i>"
      OrgText_underline e -> "<u>" &+ e &+ "</u>"
      OrgText_verbatim e -> "<code>" &+ e &+ "</code>"
      OrgText_strikethrough e -> "<s>" &+ e &+ "</s>"
      OrgText_link_target _ -> blank
      OrgText_radio_target t -> sayable @"html" t
      OrgText_export kind ls -> bool blank (toLines ls) $ kind == pack "@html"

      -- OrgText_code ls -> "\n<div class=\"code\">"
      --                    &< "<pre>"
      --                    &< toLines ls
      --                    &< "</pre>"
      --                    &< "</div>"

instance Sayable "html" [OrgText' t] => Sayable "html" (OrgLink t) where
  sayable (OrgLink l d) =
    case d of
      Nothing -> "<a href=\"" &+ l &+ "\">" &+ l &+ "</a>"
      Just t  -> "<a href=\"" &+ l &+ "\">" &+ t &+ "</a>"


hspan :: Sayable "html" n => String -> n -> Saying "html"
hspan c t = "<span class=\"" &+ c &+ "\">" &+ t &+ "</span>"
