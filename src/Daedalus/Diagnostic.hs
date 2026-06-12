{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Daedalus.Diagnostic
  (
    daedalusDiagnostic
  , diagnoseToSayableAnn
  )
where

import           Daedalus.RTS ( ParseError(..), inputOffset, inputLength )
import qualified Daedalus.RTS.Vector as DV
import           Data.Function ( on )
import           Data.Maybe ( catMaybes )
import           Data.Name
import           Data.Text ( Text )
import qualified Data.Text as T
import           Data.Text.Encoding ( decodeUtf8Lenient )
import           Error.Diagnose
import           Prettyprinter ( Pretty, Doc, reAnnotate )
import           Text.Sayable


-- | A helper function to convert a Daedalus ParseError into a Diagnostic Report.
daedalusDiagnostic :: Name "operation error"
                   -> Name "input name"
                   -> Name "input text"
                   -> ParseError
                   -> Diagnostic Text
daedalusDiagnostic opErr inpFrom inptxt pe =
  let maxLen = 1024 -- maximum # input characters to show.
      notes = concatMap (fmap Note) $ peStack pe
      loc = inputOffset $ peInput pe
      lEn = inputLength $ peInput pe
      lTx = nameText inptxt
      computeLC isEnding off =
        -- Determine the row/col for offset 'off' in input 'lTx'.  This is tricky
        -- because the row/col are 1-based, and newlines count in the offset
        -- computation (and in fact, 'off' can point at a newline).
        let go pl (r,c) 0 _ = found pl (r,c)
            go pl (r,c) o t = case T.uncons t of
              Nothing -> found pl (r,c)  -- reached end of input
              Just ('\n', tx') -> go c (r+1, 1) (o-1) tx'
              Just (_, tx') -> go pl (r, c+1) (o-1) tx'
            found pl (r,c) = if isEnding then stepBack pl (r,c) else (r,c)
            stepBack pl (r,c) =
              -- the ending location has a habit of pointing to the final
              -- newline, which causes (r,1) where r is one more than the number
              -- of lines in the file.  This causes diagnose to display that line
              -- with a "<no line>" marker, which is a bit confusing, so step the
              -- ending location back to the end of the previous line if this is
              -- the case.
                if (c > 1)
                then (r, c)  -- to previous character
                else (r-1, pl)   -- which might be the last char on previous line
        in go 99 (1,1) off lTx
      inpFrm = computeLC False loc
      inpTo = computeLC True $ loc + (min maxLen lEn)
      pos = Position inpFrm inpTo (T.unpack $ nameText inpFrom)
      posmarks = catMaybes
                 [ Just ( pos, This $ decodeUtf8Lenient $ DV.vecToRep $ peMsg pe )
                 , if T.null (peLoc pe)
                   then Nothing
                   else Just ( pos, Where $ peLoc pe )
                 ]
      forInput = addFile mempty (T.unpack $ nameText inpFrom) (T.unpack lTx)
  in addReport forInput $ Err Nothing (nameText opErr) posmarks notes


instance Pretty msg => Sayable saytag (Diagnostic msg) where
  sayable = sayable
            . diagnoseToSayableAnn
            . prettyDiagnostic WithUnicode (TabSize 8)


diagnoseToSayableAnn :: Doc (Annotation a) -> Doc SayableAnn
diagnoseToSayableAnn = reAnnotate $ \case
  ThisColor isErr -> SayableAnn $ if isErr then "error" else "warning"
  MaybeColor -> SayableAnn "suggestion"
  WhereColor -> SayableAnn "reason"
  HintColor -> SayableAnn "suggestion"
  FileColor -> SayableAnn "location_path"
  RuleColor -> SayableAnn "line_subportion"
  KindColor isErr -> SayableAnn $ if isErr then "error" else "warning"
  NoLineColor -> SayableAnn "ann_LocStartLine"
  MarkerStyle _ -> SayableAnn "statement_subportion"
  CodeStyle -> SayableAnn "statement_unselected"
  OtherStyle _ -> SayableAnn "other"


----------------------------------------------------------------------


-- n.b. to use Diagnostic as the error type component (FancyError) in a
-- megaparsec parse, there must be an Ord instance (and therefore an Eq
-- instance), but Diagnostic does not supply these.  It is not entirely clear
-- that the Ord instance is important; at some point FancyError is a Set of these
-- error type components, presumably for de-duplication, but is there another
-- reason?

instance Pretty msg => Eq (Diagnostic msg) where
  (==) = (==) `on` sez @"error"

instance Pretty msg => Ord (Diagnostic msg) where
  compare = compare `on` sez @"error"
