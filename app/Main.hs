{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import           Data.Name
import           Data.String ( fromString )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Version ( showVersion )
import           Options.Applicative
import           Options.Applicative.Help ( paragraph, vsepChunks )
import           OrgMode
import           Paths_orgmode_daedalus ( version )
import           System.IO
import           System.Exit
import           Text.Sayable


data Params = Params { exportFormat :: Maybe (Name "format")
                     , outFile :: Maybe (Name "output filename")
                     , inpFile :: Name "input name"
                     }
            deriving Show

params :: Parser Params
params = Params
         <$> optional
             (option str (long "format" <> short 'f'
                          <> help "Output format (default = parsed AST)"
                         ))
         <*> optional
             (option str (long "output" <> short 'o'
                          <> help "Output filename (default = stdout)"
                         ))
         <*> argument str (metavar "INPUT-FILE"
                           <> help "Input org-mode file to process"
                          )

formats :: [(Name "format", OrgDoc -> String)]
formats = [ (fromString "text", sez @"text")
          , (fromString "text-style1", sez @"text-style1")
          , (fromString "html", sez @"html")
          ]

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
              case exportFormat args of
                Nothing -> emit args $ show parsed
                Just f -> case lookup f formats of
                  Just emitter -> emit args $ emitter parsed
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
               <**> simpleVersioner ('v':showVersion version)
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
