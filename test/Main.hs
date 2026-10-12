{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import           Data.Maybe ( fromMaybe )
import           Data.String ( fromString )
import qualified Data.Text as T
import           Numeric.Natural
import           Path
import           Test.Tasty
import           Test.Tasty.Checklist ( multiLineDiff )
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients ( Ingredient )
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PPS
import           Test.Tasty.Runners.AntXML
import           Test.Tasty.Sugar
import qualified TestOrgMode
import           Text.Sayable

import           OrgMode


sugarCube :: CUBE
sugarCube = mkCUBE { inputDirs = [ "test/expected" ]
                   , rootName = "*.org"
                   , expectedSuffix = ".exp"
                   , validParams = [ ("format", Just [ "html"
                                                     , "text"
                                                     , "textstyle1"
                                                     ])
                                   ]
                   }

ingredients :: [Ingredient]
ingredients = includingOptions sugarOptions
              : antXMLRunner
              : sugarIngredients [sugarCube]
              <> defaultIngredients

main :: IO ()
main = tests >>= defaultMainWithIngredients ingredients


mkTest :: Sweets -> Natural -> Expectation -> IO [TestTree]
mkTest sweets n expct =
  do let fmt = fromMaybe "text"
               (getParamVal =<< lookup "format" (expParamsMatch expct))
     let testName = sez @"test" $ rootMatchName sweets &- '#' &+ n &- fmt
     return
       [ testCase testName
       $ do expectedExport <- readFile $ expectedFile expct
            inp <- fromString <$> (readFile $ rootFile sweets)
            let inpName = fromString $ rootBaseName sweets
            orgdoc <- case orgStructureParse inpName inp "bad org file" of
                        Left err -> error $ sez @"error" $ t'"Parse failed:" &- err
                        Right parsed ->
                          do let fdir = maybe (Rel [reldir|.|])
                                        (mapSomeBase parent)
                                        $ parseSomeFile $ expectedFile expct
                             (sts, p2) <- insertAllInclusions fdir parsed
                             let isInsertErr = \case
                                   Included {} -> False
                                   _ -> True
                             case filter isInsertErr sts of
                               [] -> return p2
                               es -> error $ sez @"error"
                                     $ t'"Inclusion processing failed:" &* es
            let orgExp =
                  case fmt of
                    "text" -> sez @"text" orgdoc
                    "html" -> sez @"html" orgdoc
                    "textstyle1" -> PPS.renderString
                                    $ PP.layoutPretty (PP.LayoutOptions
                                                       $ PP.AvailablePerLine 72 1.0)
                                    $ saying
                                    $ sayable @"text-style1" orgdoc
                    _ -> error $ "Unknown format parameter value: "<> fmt
            if orgExp == expectedExport
              then orgExp @?= expectedExport
              else assertFailure ("did not match for: " <> expectedFile expct
                                  <> "\n\nActual:\n"
                                  <> orgExp
                                  <> "\n\nDiff expected->actual:\n"
                                  <> multiLineDiff (T.pack expectedExport)
                                  (T.pack orgExp)
                                 )
       ]


tests :: IO TestTree
tests =
  do testSweets <- findSugar sugarCube
     hspecTests <- sequence [ TestOrgMode.testOrgMode ]
     exportTests <- withSugarGroups testSweets testGroup mkTest
     return $ testGroup "OrgMode"
       [ testGroup "Parsing" hspecTests
       , testGroup "Export" exportTests
       ]
