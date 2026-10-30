{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import           Data.Bool ( bool )
import qualified Data.List as L
import           Data.Name
import           Data.String ( fromString )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Version ( showVersion )
import           Numeric.Natural ( Natural )
import           Options.Applicative
import           Options.Applicative.Help ( paragraph, vsepChunks )
import           OrgMode
import qualified Path as P
import qualified Path.IO as PIO
import           Paths_orgmode_daedalus ( version )
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Terminal as PR
import qualified Prettyprinter.Render.Text as PT
import           System.Exit
import           System.IO
import           Text.Sayable


data Params = Params { exportFormat :: Maybe (Name "format")
                     , outWidth :: Natural
                     , outFile :: Maybe (Name "output filename")
                     , noANSI :: Bool
                     , inpFile :: Name "input name"
                     }
            deriving Show

params :: Bool -> Parser Params
params isTerm =
  Params
  <$> optional (option str (long "format" <> short 'f'
                            <> help (unwords
                                     [ "Output format (default = parsed AST)."
                                     , "Valid:"
                                     , (L.intercalate ", "
                                        (T.unpack . nameText . fst <$> formats))
                                     ]
                                    )
                           ))
  <*> (option auto (long "width" <> short 'w'
                    <> help "Output width, if applicable"
                    <> showDefault
                    <> value 74
                   ))
  <*> optional (option str (long "output" <> short 'o'
                            <> help "Output filename (default = stdout)"
                           ))
  <*> switch (long "no-ansi" <> short 'p'
              <> help "Plain: no ANSI control sequences"
              <> showDefault
              -- if already plain, don't even show this
              <> bool internal showDefault isTerm
             )
  <*> argument str (metavar "INPUT-FILE"
                    <> help "Input org-mode file to process"
                   )

formats :: [(Name "format", Params -> OrgDoc -> T.Text)]
formats = [ (fromString "text", textEmitter @"text")
          , (fromString "text-style1", textEmitter @"text-style1")
          , (fromString "html", const $ T.pack . sez @"html")
          ]

textEmitter :: forall format . Sayable format OrgDoc => Params -> OrgDoc -> T.Text
textEmitter p doc =
  let w = max 10 $ fromEnum $ outWidth p
      opts = PP.LayoutOptions $ PP.AvailablePerLine w 0.95
      doc' = PP.layoutSmart opts $ saying $ sayable @format doc
  in if noANSI p
     then PT.renderStrict doc'
     else PR.renderStrict $ PP.reAnnotateS colTermAnn doc'
  where
    colTermAnn :: SayableAnn -> PR.AnsiStyle
    colTermAnn = \case
      SayableAnn "bold" -> PR.bold
      SayableAnn "italics" -> PR.italicized
      SayableAnn "underline" -> PR.underlined
      SayableAnn "verbatim" -> PR.bgColor PR.Blue <> PR.color PR.Yellow
      SayableAnn "strikethrough" -> PR.colorDull PR.Black
      SayableAnn "code" -> PR.bgColor PR.Black <> PR.color PR.Green
      SayableAnn "example" -> PR.bgColor PR.Black <> PR.color PR.White
      SayableAnn "link" -> PR.color PR.Blue
      SayableAnn "item" -> PR.color PR.Magenta
      SayableAnn "todo" -> PR.colorDull PR.Red
      SayableAnn "done" -> PR.color PR.Green
      _ -> mempty

main :: IO ()
main = do isTerm <- hIsTerminalDevice stdout
          args <- customExecParser (prefs showHelpOnError) (opts isTerm)
          inp <- fromText <$> TIO.readFile (T.unpack $ nameText $ inpFile args)
          let errmsg = fromString "bad org file"
          let p = orgStructureParse (inpFile args) inp errmsg
          case p of
            Left err ->
              do hPutStrLn stderr $ sez @"error" $ t'"Parse failed:" &- err
                 exitFailure
            Right parsed ->
              do here <- P.Abs <$> PIO.getCurrentDir
                 (sts,doc) <- insertAllInclusions here parsed
                 mapM_ (hPutStrLn stderr . sez @"error") sts
                 case exportFormat args of
                   Nothing -> emit args $ T.pack $ show doc
                   Just f ->
                     case lookup f formats of
                       Just emitter -> emit args $ emitter args doc
                       Nothing ->
                         do hPutStrLn stderr $ sez @"error"
                              $ t'"Unknown format:" &- f
                            exitFailure
          exitSuccess
  where
    emit args out = case outFile args of
                      Nothing -> TIO.putStrLn out
                      Just f -> TIO.writeFile (T.unpack $ nameText f) out
    opts p = (info
              (params p
#if MIN_VERSION_optparse_applicative(0,17,1)
               <**> simpleVersioner ('v':showVersion version)
#else
               <**> (infoOption ('v':showVersion version)
                     $ mconcat [ long "version"
                               , help "Show version information"
                               , hidden
                               ]
                    )
#endif
               <**> helper
              )
             ((fullDesc
               <> progDesc "Run the org-mode parser/exporter tool"
               <> header "orgmode - Tool for parsing and exporting Emacs https://orgmode.org files"
              )))
              {
                infoFooter = (vsepChunks
                            [ paragraph $ unlines
                              [ "This tool can be used to parse and export an org-mode"
                              , "file.  Several export formats are supported."
                              ]
                            ]
                         )
              }
