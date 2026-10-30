{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
import qualified Prettyprinter.Render.String as PS
import           System.Exit
import           System.IO
import           Text.Sayable


data Params = Params { exportFormat :: Maybe (Name "format")
                     , outWidth :: Natural
                     , outFile :: Maybe (Name "output filename")
                     , inpFile :: Name "input name"
                     }
            deriving Show

params :: Parser Params
params = Params
         <$> optional
             (option str (long "format" <> short 'f'
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
         <*> optional
             (option str (long "output" <> short 'o'
                          <> help "Output filename (default = stdout)"
                         ))
         <*> argument str (metavar "INPUT-FILE"
                           <> help "Input org-mode file to process"
                          )

formats :: [(Name "format", Params -> OrgDoc -> String)]
formats = [ (fromString "text", textEmitter @"text")
          , (fromString "text-style1", textEmitter @"text-style1")
          , (fromString "html", const $ sez @"html")
          ]

textEmitter :: forall format . Sayable format OrgDoc => Params -> OrgDoc -> String
textEmitter p doc =
  let w = max 10 $ fromEnum $ outWidth p
      opts = PP.LayoutOptions $ PP.AvailablePerLine w 0.95
      doc' = PP.layoutSmart opts $ saying $ sayable @format doc
  in PS.renderString doc'

main :: IO ()
main = do args <- customExecParser (prefs showHelpOnError) opts
          inp <- fromText <$> TIO.readFile (T.unpack $ nameText $ inpFile args)
          let errmsg = fromString "bad org file"
          let p = orgStructureParse (inpFile args) inp errmsg
          case p of
            Left err ->
              do hPutStrLn stderr $ sez @"error" $ "Parse failed:" &- err
                 exitFailure
            Right parsed ->
              do here <- P.Abs <$> PIO.getCurrentDir
                 (sts,doc) <- insertAllInclusions here parsed
                 mapM_ (hPutStrLn stderr . sez @"error") sts
                 case exportFormat args of
                   Nothing -> emit args $ show doc
                   Just f ->
                     case lookup f formats of
                       Just emitter -> emit args $ emitter args doc
                       Nothing ->
                         do hPutStrLn stderr $ sez @"error" $ "Unknown format:" &- f
                            exitFailure
          exitSuccess
  where
    emit args out = case outFile args of
                      Nothing -> putStrLn out
                      Just f -> writeFile (T.unpack $ nameText f) out
    opts = (info
             (params
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
