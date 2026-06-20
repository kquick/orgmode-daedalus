{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

{- |
Module       : TestOrgMOde
Description  : Taphos source
Copyright    : (c) 2026, Galois, Inc.
License      : BSD3
Stability    : provisional
Portability  : portable
-}

module TestOrgMode
  (
    testOrgMode
  )
where

import           Control.Monad ( (<=<) )
import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Catch ( MonadMask )
import           Data.Char ( chr, ord )
import qualified Data.Foldable as F
import           Data.Maybe ( isJust )
import           Data.Name
import           Data.Parameterized.Context ( pattern Empty, pattern (:>) )
import           Data.String ( fromString )
import           Data.Text ( Text )
import qualified Data.Text as T
import           GHC.Records ( HasField, getField )
import           Numeric ( showHex )

import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Checklist
import           Test.Tasty.Hspec

import           Daedalus.RTS
import qualified Daedalus.RTS.Vector as DV

import           OrgMode
import           Text.Sayable


instance {-# OVERLAPPABLE #-} TestShow a => TestShow [a] where
  testShow = testShowList
instance {-# OVERLAPPABLE #-} (TestShow a, DV.VecElem a
                              ) => TestShow (DV.Vector a) where
  testShow = testShow . DV.toList
instance {-# OVERLAPPING #-} TestShow (DV.Vector (UInt 8)) where
  testShow = testShow . DV.vecToString

instance TestShow a => TestShow (Maybe a) where
  testShow = \case
    Just a -> "J:" <> testShow a
    Nothing -> "NO"

instance TestShow Text where testShow = show
instance (TestShow a, TestShow b) => TestShow (Either a b) where
  testShow = \case
    Left l -> "L> " <> testShow l
    Right r -> "R> " <> testShow r
instance TestShow ParseError where testShow = show
instance TestShow OrgDoc where testShow = show
instance TestShow OrgSection where testShow = show
instance TestShow OrgBody where testShow = show
instance TestShow OrgText where testShow = show
instance TestShow ListItem where testShow = show
instance TestShow Drawer where testShow = show
instance TestShow Block where testShow = show
instance TestShow (UInt 8) where
  testShow v = shows v
               (("/0x" :: String) <> showHex (asInt v)
                ("/'" <> (maybe '?' chr (toInt $ asInt v) : "'")))

atVIdx :: DV.VecElem a => Int -> DV.Vector a -> Maybe a
atVIdx i l = l DV.!? lit (toInteger i)

-- Standard naming and setup of a test
verify :: (HasCallStack, Example (m a), MonadIO m, MonadMask m)
       => Name "test name"
       -> Name "test description"
       -> (CanCheck => Name "input name" -> m a)
       -> SpecWith (Arg (m a))
verify nm desc tst =
  let inpName = fromString $ sez @"test" $ t'"Test" &- nameText nm &- t'"input"
                :: Name "input name"
  in it (sez @"test" $ nameText nm &- '-' &- nameText desc)
     $ withChecklist (nameText nm)
     $ tst inpName

-- Run the parser and verify the result is good and passes the tests
parseTest inp tests nm =
  case orgStructureParse nm inp "input parsing test" of
    Left e -> error $ sez @"error" $ t'"Parse failed:" &- e
    Right o -> o `checkValues` tests

-- This is verify + parseTest applied multiple times to:
--   1. Stripped input
--   2. Input with various blank-line prefixes
--   3. Input with various blank-line suffixes
--   4. Input with blank lines before and after
verifyParse nm desc inp tests =
  let grpName = sez @"test" $ nameText (nm :: Name "test name")
                &- '-' &- nameText (desc :: Name "test description")
      pre = [ "", "\n  \n\n", "\n", " \n", "\n \n\n\n" ]
      post = [ "", "\n", "\n\n", "\n \n", "\n \n   \n" ]
      base = fromText $ T.strip $ nameText (inp :: Name "input text")
      baseName = sez @"test" $ nameText nm &+ t'".base"
      both = fromText $ pre !! 4 <> nameText base <> post !! 4
      bothName = sez @"test" $ nameText nm &+ t'".prefixsuffix"
  in describe grpName $ do
      it baseName
        $ withChecklist (T.pack baseName)
        $ parseTest base tests (fromString baseName)
      mapM_ (\(i,pfx) ->
               let tname = sez @"test" $ nameText nm &+ t'".prefix" &+ i
               in it tname
                  $ withChecklist (T.pack tname)
                  $ parseTest (fromText (pfx <> nameText base)) tests (fromString tname)
            ) $ zip [(1::Int)..] pre
      mapM_ (\(i,sfx) ->
               let tname = sez @"test" $ nameText nm &+ t'".suffix" &+ i
               in it tname
                  $ withChecklist (T.pack tname)
                  $ parseTest (fromText (nameText base <> sfx)) tests (fromString tname)
            ) $ zip [(1::Int)..] post
      it bothName
        $ withChecklist (T.pack bothName)
        $ parseTest both tests (fromString bothName)


----------------------------------------------------------------------

testOrgMode :: IO TestTree
testOrgMode = testSpec "OrgMode" $ do

  describe "empty doc" $ do

    verify "Org01" "parse an empty document" $
      parseTest ""
      (Empty
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
       :> Val "main body size" (F.length . DV.toList . getField @"body") 0
      )

    verify "Org02" "parse a whitespace document" $
      parseTest "    \t\n\n\n\t\t  \t\n \n   \n  "
      (Empty
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
       :> Val "main body size" (F.length . DV.toList . getField @"body") 0
      )

  describe "body elements" $ do

    verifyParse "Org10" "parse a one word paragraph"
      "hello"
      (Empty
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
       :> Val "main body" getBody [toPara ["hello"]]
      )

    verifyParse "Org11" "parse a multi-word paragraph"
      "hello world, I am alive!  Can you read this?"
      (Empty
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
       :> Val "main body" getBody
        [
          toPara [ "hello world, I am alive! Can you read this?" ]
        ]
      )

    verifyParse "Org12" "parse a multi-line paragraph"
      "hello world, I am alive!\nCan you read this?"
      (Empty
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
       :> Val "main body" getBody
        [
          toPara [ "hello world, I am alive!"
                 , "Can you read this?"
                 ]
        ]
      )

    verifyParse "Org13" "parse a multi-line paragraph, internal ws"
      "hello world, I am alive!\n  Can you read this?"
      (Empty
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
       :> Val "main body" getBody
        [
          toPara [ "hello world, I am alive!"
                 , "Can you read this?"
                 ]
        ]
      )

    verify "Org14" "parse multiple paragraphs" $
      parseTest "hello world,\nI am alive!\n\nCan you\nread this?"
      (Empty
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
       :> Val "main body" getBody
        [
          toPara [ "hello world,"
                 , "I am alive!"
                 ]
        , toPara [ "Can you"
                 , "read this?"
                 ]
        ]
      )

    verify "Org15" "parse multiple paragraphs + ws" $
      parseTest
      "hello world,\n I am alive!\n  \n    Can you\nread this?"
      (Empty
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
       :> Val "main body" getBody
        [
          toPara [ "hello world,"
                 , "I am alive!"
                 ]
        , toPara [ "Can you"
                 , "read this?"
                 ]
        ]
      )

    verify "Org16" "parse multiple paragraphs with splats" $
      parseTest
      "hello world,\n I *am* alive!\n  \n    Can ** you **\nread * this?"
      (Empty
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
       :> Val "main body" getBody
        [
          toPara [ "hello world,"
                 , "I *am* alive!"
                 ]
        , toPara [ "Can ** you **"
                 , "read * this?"
                 ]
        ]
      )

  ----------------------------------------------------------------------

  describe "sections" $ do

    verify "Org60" "parse header only" $
      parseTest
      "* section"
      (Empty
       :> Val "main body" getBody []
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 1
       :> Val "section 0 header" (getField @"header" . getSection 0)
        (toLine "section")
       :> Val "section 0 level" (asInt . getField @"level" . getSection 0) 1
       :> Val "section 0 body" (getField @"body" . getSection 0)
        (DV.fromList [])
       :> Val "section 0 subsections" (getField @"sections" . getSection 0)
        (DV.fromList [])
      )

    verifyParse "Org61" "parse multi-word header only"
      "* section title is multiple words"
      (Empty
       :> Val "main body" getBody []
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 1
       :> Val "section 0 header" (getField @"header". getSection 0)
        (toLine "section title is multiple words")
       :> Val "section 0 body" (getField @"body" . getSection 0)
        (DV.fromList [])
       :> Val "section 0 subsections" (getField @"sections" . getSection 0)
        (DV.fromList [])
      )

    verifyParse "Org62" "parse multi-word header and paras"
      "* section title is multiple words\nFirst paragraph.\n \nSecond\npara"
      (Empty
       :> Val "main body" getBody []
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 1
       :> Val "section 0 header" (getField @"header" . getSection 0)
        (toLine "section title is multiple words")
       :> Val "section 0 body" (getBody . getSection 0)
        [
          toPara [ "First paragraph." ]
        , toPara [ "Second"
                 , "para"
                 ]
        ]
       :> Val "section 0 subsections" (getField @"sections" . getSection 0)
        (DV.fromList [])
      )

    verifyParse "Org63a" "multiple sections"
      (fromString
       $ unlines [ "* section title"
                 , "* Second section"
                 , ""
                 , "* third section"
                 ]
      )
      (Empty
       :> Val "main body" getBody []
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 3
       :> Val "section 0 header" (getField @"header" . getSection 0)
        (toLine "section title")
       :> Val "section 0 level" (asInt . getField @"level" . getSection 0) 1
       :> Val "section 0 body" (getField @"body" . getSection 0)
        (DV.fromList [])
       :> Val "section 0 subsections" (getField @"sections" . getSection 0)
        (DV.fromList [])
       :> Val "section 1 header" (getField @"header" . getSection 1)
        (toLine "Second section")
       :> Val "section 1 level" (asInt . getField @"level" . getSection 1) 1
       :> Val "section 1 body" (getField @"body" . getSection 1)
        (DV.fromList [])
       :> Val "section 1 subsections" (getField @"sections" . getSection 1)
        (DV.fromList [])
       :> Val "section 2 header" (getField @"header" . getSection 2)
        (toLine "third section")
       :> Val "section 2 level" (asInt . getField @"level" . getSection 2) 1
       :> Val "section 2 body" (getField @"body" . getSection 2)
        (DV.fromList [])
       :> Val "section 2 subsections" (getField @"sections" . getSection 2)
        (DV.fromList [])
      )

    verifyParse "Org63b" "multiple only subsections"
      (fromString
       $ unlines [ "** first"
                 , "    text"
                 , "** last "
                 , ""
                 ]
      )
      (Empty
       :> Val "main body" getBody []
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 1
       :> Val "section 0 header" (getField @"header" . getSection 0)
        (toLine "")
       :> Val "section 0 level" (asInt . getField @"level" . getSection 0) 1
       :> Val "section 0 body" (getField @"body" . getSection 0)
        (DV.fromList [])

       :> Val "section 0 subsectioncount" (F.length . DV.toList
                                           . getField @"sections"
                                           . getSection 0)
        2

       :> Val "section 0 sub 0 header" (getField @"header"
                                        . getSection 0
                                        . getSection 0)
        (toLine "first")
       :> Val "section 0 sub 0 level" (asInt . getField @"level"
                                       . getSection 0
                                       . getSection 0) 2
       :> Val "section 0 sub 0 body" (getField @"body"
                                      . getSection 0
                                      . getSection 0)
        (DV.fromList [ toPara ["text"] ])

       :> Val "section 0 sub 1 header" (getField @"header"
                                        . getSection 1
                                        . getSection 0)
        (toLine "last")
       :> Val "section 0 sub 1 level" (asInt . getField @"level"
                                       . getSection 1
                                       . getSection 0) 2
       :> Val "section 0 sub 1 body" (getField @"body"
                                      . getSection 1
                                      . getSection 0)
        (DV.fromList [ ])
      )

    verifyParse "Org64" "multiple subsections"
      (fromString
       $ unlines [ "* section title"
                 , "* Second section"
                 , "** subsection"
                 , ""
                 , "** subsection 2"
                 , ""
                 , ""
                 , ""
                 , "** subsection3"
                 , "*** subsubsection 1"
                 , "**** subsubsubsection"
                 , ""
                 , "* third section"
                 , "** sub to third"
                 ]
      )
      (Empty
       :> Val "main body" getBody []
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 3
       :> Val "section 0 header" (getField @"header" . getSection 0)
        (toLine "section title")
       :> Val "section 0 level" (asInt . getField @"level" . getSection 0) 1
       :> Val "section 0 body" (getField @"body" . getSection 0)
        (DV.fromList [])
       :> Val "section 0 subsections" (getField @"sections" . getSection 0)
        (DV.fromList [])
       :> Val "section 1 header" (getField @"header" . getSection 1)
        (toLine "Second section")
       :> Val "section 1 level" (asInt . getField @"level" . getSection 1) 1
       :> Val "section 1 body" (getField @"body" . getSection 1)
        (DV.fromList [])
       :> Val "num section 1 subsections" (F.length . DV.toList
                                           . getField @"sections"
                                           . getSection 1) 3

       :> Val "section 1 sub 0 header" (getField @"header"
                                        . getSection 0
                                        . getSection 1)
        (toLine "subsection")
       :> Val "section 1 sub 0 level" (asInt . getField @"level"
                                       . getSection 0
                                       . getSection 1) 2
       :> Val "section 1 sub 0 body" (getField @"body"
                                        . getSection 0
                                        . getSection 1)
        (DV.fromList [])
       :> Val "section 1 sub 0 num subsections" (F.length . DV.toList
                                                 . getField @"sections"
                                                 . getSection 0
                                                 . getSection 1) 0

        :> Val "section 1 sub 1 header" (getField @"header"
                                        . getSection 1
                                        . getSection 1)
        (toLine "subsection 2")
       :> Val "section 1 sub 1 level" (asInt . getField @"level"
                                       . getSection 1
                                       . getSection 1) 2
       :> Val "section 1 sub 1 body" (getField @"body"
                                        . getSection 1
                                        . getSection 1)
        (DV.fromList [])
       :> Val "section 1 sub 1 num subsections" (F.length . DV.toList
                                                 . getField @"sections"
                                                 . getSection 1
                                                 . getSection 1) 0

        :> Val "section 1 sub 2 header" (getField @"header"
                                        . getSection 2
                                        . getSection 1)
        (toLine "subsection3")
       :> Val "section 1 sub 2 level" (asInt . getField @"level"
                                       . getSection 2
                                       . getSection 1) 2
       :> Val "section 1 sub 2 body" (getField @"body"
                                        . getSection 2
                                        . getSection 1)
        (DV.fromList [])
       :> Val "section 1 sub 2 num subsections" (F.length . DV.toList
                                                 . getField @"sections"
                                                 . getSection 2
                                                 . getSection 1) 1

       :> Val "section 1 sub 2 sub 0 header" (getField @"header"
                                              . getSection 0
                                              . getSection 2
                                              . getSection 1)
        (toLine "subsubsection 1")
       :> Val "section 1 sub 0 level" (asInt . getField @"level"
                                       . getSection 0
                                       . getSection 2
                                       . getSection 1) 3
       :> Val "section 1 sub 2 sub 0 body" (getField @"body"
                                            . getSection 0
                                            . getSection 2
                                            . getSection 1)
        (DV.fromList [])
       :> Val "section 1 sub 2 sub 0 num subsections" (F.length . DV.toList
                                                       . getField @"sections"
                                                       . getSection 0
                                                       . getSection 2
                                                       . getSection 1) 1

       :> Val "section 1 sub 2 sub 0 sub 0 header" (getField @"header"
                                                    . getSection 0
                                                    . getSection 0
                                                    . getSection 2
                                                    . getSection 1)
        (toLine "subsubsubsection")
       :> Val "section 1 sub 0 level" (asInt . getField @"level"
                                       . getSection 0
                                       . getSection 0
                                       . getSection 2
                                       . getSection 1) 4
       :> Val "section 1 sub 2 sub 0 sub 0 body" (getField @"body"
                                                  . getSection 0
                                                  . getSection 0
                                                  . getSection 2
                                                  . getSection 1)
        (DV.fromList [])
       :> Val "section 1 sub 2 sub 0 sub 0 num subsections"
        (F.length . DV.toList
         . getField @"sections"
         . getSection 0
         . getSection 0
         . getSection 2
         . getSection 1) 0


       :> Val "section 2 header" (getField @"header" . getSection 2)
        (toLine "third section")
       :> Val "section 2 body" (getField @"body" . getSection 2)
        (DV.fromList [])
       :> Val "num section 2 subsections" (F.length . DV.toList
                                           . getField @"sections"
                                           . getSection 2) 1
      )

    verifyParse "Org65" "para + section hdr"
      (fromString $ unlines [ "This is a paragraph."
                            , "It has two lines."
                            , "* This is a section"
                            ])
      (Empty
       :> Val "main body" getBody [ toPara [ "This is a paragraph."
                                           , "It has two lines."
                                           ]
                                  ]
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 1
       :> Val "section 0 header" (getField @"header" . getSection 0)
        (toLine "This is a section")
       :> Val "section 0 body" (getField @"body" . getSection 0)
        (DV.fromList [])
       :> Val "section 0 subsections" (getField @"sections" . getSection 0)
        (DV.fromList [])
      )

    verifyParse "Org66" "section hdr + para"
      (fromString $ unlines [ "* This is a section"
                            , "This is a paragraph."
                            , "It has two lines."
                            ])
      (Empty
       :> Val "main body" getBody []
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 1
       :> Val "section 0 header" (getField @"header" . getSection 0)
        (toLine "This is a section")
       :> Val "section 0 body" (getField @"body" . getSection 0)
        (DV.fromList [ toPara [ "This is a paragraph."
                              , "It has two lines."
                              ]
                     ])
       :> Val "section 0 subsections" (getField @"sections" . getSection 0)
        (DV.fromList [])
      )

    verify "Org67" "section hdr + bl + para" $
      parseTest (fromString $ unlines [ "* This is a section"
                                      , ""
                                      , "This is a paragraph."
                                      , "It has two lines."
                                      ])
      (Empty
       :> Val "main body" getBody []
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 1
       :> Val "section 0 header" (getField @"header" . getSection 0)
        (toLine "This is a section")
       :> Val "section 0 body" (getField @"body" . getSection 0)
        (DV.fromList [ toPara [ "This is a paragraph."
                              , "It has two lines."
                              ]
                     ])
       :> Val "section 0 subsections" (getField @"sections" . getSection 0)
        (DV.fromList [])
      )

    verifyParse "Org68" "multiple subsections with para"
      (fromString $ unlines [ "* section title"
                            , "* Second section"
                            , "This is a paragraph"
                            , "** subsection"
                            , ""
                            , "A sub-paragraph."
                            , ""
                            , "** subsection 2"
                            , ""
                            , ""
                            , ""
                            , "** subsection3"
                            , "*** subsubsection 1"
                            , "  subpara 1"
                            , ""
                            , "subpara 2"
                            , "  on multiple"
                            , "   lines"
                            , ""
                            , "     subpara 3"
                            , "**** subsubsubsection"
                            , ""
                            , "* third section"
                            , "** sub to third"
                            ]
      )
      (Empty
       :> Val "main body" getBody []
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 3
       :> Val "section 0 header" (getField @"header" . getSection 0)
        (toLine "section title")
       :> Val "section 0 body" (getField @"body" . getSection 0)
        (DV.fromList [])
       :> Val "section 0 subsections" (getField @"sections" . getSection 0)
        (DV.fromList [])
       :> Val "section 1 header" (getField @"header" . getSection 1)
        (toLine "Second section")
       :> Val "section 1 body" (getField @"body" . getSection 1)
        (DV.fromList [ toPara [ "This is a paragraph" ] ])
       :> Val "num section 1 subsections" (F.length . DV.toList
                                           . getField @"sections"
                                           . getSection 1) 3

       :> Val "section 1 sub 0 header" (getField @"header"
                                        . getSection 0
                                        . getSection 1)
        (toLine "subsection")
       :> Val "section 1 sub 0 body" (getField @"body"
                                        . getSection 0
                                        . getSection 1)
        (DV.fromList [ toPara [ "A sub-paragraph." ] ])
       :> Val "section 1 sub 0 num subsections" (F.length . DV.toList
                                                 . getField @"sections"
                                                 . getSection 0
                                                 . getSection 1) 0

        :> Val "section 1 sub 1 header" (getField @"header"
                                        . getSection 1
                                        . getSection 1)
        (toLine "subsection 2")
       :> Val "section 1 sub 1 body" (getField @"body"
                                        . getSection 1
                                        . getSection 1)
        (DV.fromList [])
       :> Val "section 1 sub 1 num subsections" (F.length . DV.toList
                                                 . getField @"sections"
                                                 . getSection 1
                                                 . getSection 1) 0

        :> Val "section 1 sub 2 header" (getField @"header"
                                        . getSection 2
                                        . getSection 1)
        (toLine "subsection3")
       :> Val "section 1 sub 2 body" (getField @"body"
                                        . getSection 2
                                        . getSection 1)
        (DV.fromList [])
       :> Val "section 1 sub 2 num subsections" (F.length . DV.toList
                                                 . getField @"sections"
                                                 . getSection 2
                                                 . getSection 1) 1

       :> Val "section 1 sub 2 sub 0 header" (getField @"header"
                                              . getSection 0
                                              . getSection 2
                                              . getSection 1)
        (toLine "subsubsection 1")
       :> Val "section 1 sub 2 sub 0 body" (getField @"body"
                                            . getSection 0
                                            . getSection 2
                                            . getSection 1)
        (DV.fromList [ toPara [ "subpara 1"]
                     , toPara [ "subpara 2", "on multiple", "lines" ]
                     , toPara [ "subpara 3" ]
                     ])
       :> Val "section 1 sub 2 sub 0 num subsections" (F.length . DV.toList
                                                       . getField @"sections"
                                                       . getSection 0
                                                       . getSection 2
                                                       . getSection 1) 1

       :> Val "section 1 sub 2 sub 0 sub 0 header" (getField @"header"
                                                    . getSection 0
                                                    . getSection 0
                                                    . getSection 2
                                                    . getSection 1)
        (toLine "subsubsubsection")
       :> Val "section 1 sub 2 sub 0 sub 0 body" (getField @"body"
                                                  . getSection 0
                                                  . getSection 0
                                                  . getSection 2
                                                  . getSection 1)
        (DV.fromList [])
       :> Val "section 1 sub 2 sub 0 sub 0 num subsections"
        (F.length . DV.toList
         . getField @"sections"
         . getSection 0
         . getSection 0
         . getSection 2
         . getSection 1) 0


       :> Val "section 2 header" (getField @"header" . getSection 2)
        (toLine "third section")
       :> Val "section 2 body" (getField @"body" . getSection 2)
        (DV.fromList [])
       :> Val "num section 2 subsections" (F.length . DV.toList
                                           . getField @"sections"
                                           . getSection 2) 1
      )

    verifyParse "Org69" "multiple subsections with skipping and TODO"
      (fromString $ unlines [ "* section title"
                            , "* TODO [#B] Second section"
                            , "This is a paragraph"
                            , "**** DONE subsubsubsection"
                            , ""
                            , "* Done third section"
                            , "** Todo sub to third"
                            ])
      (Empty
       :> Val "main body" getBody []
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 3
       :> Val "section 0 header" (getField @"header" . getSection 0)
        (toLine "section title")
       :> Val "section 0 level" (asInt . getField @"level" . getSection 0) 1
       :> Val "section 0 todo" (getField @"todo" . getSection 0) Nothing
       :> Val "section 0 priority" (getField @"priority" . getSection 0) Nothing
       :> Val "section 0 body" (getField @"body" . getSection 0)
        (DV.fromList [])
       :> Val "section 0 subsections" (getField @"sections" . getSection 0)
        (DV.fromList [])
       :> Val "section 1 header" (getField @"header" . getSection 1)
        (toLine "Second section")
       :> Val "section 1 level" (asInt . getField @"level" . getSection 1) 1
       :> Val "section 1 todo" (getField @"todo" . getSection 1)
        (Just $ toWord "TODO")
       :> Val "section 1 priority" (getField @"priority" . getSection 1)
       (Just $ lit $ toInteger $ ord 'B')
              :> Val "section 1 body" (getField @"body" . getSection 1)
        (DV.fromList [ toPara [ "This is a paragraph" ] ])
       :> Val "num section 1 subsections" (F.length . DV.toList
                                           . getField @"sections"
                                           . getSection 1) 1

        :> Val "section 1 sub 0 header" (getField @"header"
                                        . getSection 0
                                        . getSection 1)
        (toLine "")
       :> Val "section 1 sub 0 body" (getField @"body"
                                        . getSection 0
                                        . getSection 1)
        (DV.fromList [])
       :> Val "section 1 sub 0 num subsections" (F.length . DV.toList
                                                 . getField @"sections"
                                                 . getSection 0
                                                 . getSection 1) 1

       :> Val "section 1 sub 0 sub 0 header" (getField @"header"
                                              . getSection 0
                                              . getSection 0
                                              . getSection 1)
        (toLine "")
       :> Val "section 1 sub 0 sub 0 body" (getField @"body"
                                            . getSection 0
                                            . getSection 0
                                            . getSection 1)
        (DV.fromList [])
       :> Val "section 1 sub 0 sub 0 num subsections" (F.length . DV.toList
                                                       . getField @"sections"
                                                       . getSection 0
                                                       . getSection 0
                                                       . getSection 1) 1

       :> Val "section 1 sub 0 sub 0 sub 0 header" (getField @"header"
                                                    . getSection 0
                                                    . getSection 0
                                                    . getSection 0
                                                    . getSection 1)
        (toLine "subsubsubsection")
       :> Val "section 1 sub 0 sub 0 sub 0 level" (asInt . getField @"level"
                                                   . getSection 0
                                                   . getSection 0
                                                   . getSection 0
                                                   . getSection 1) 4
       :> Val "section 1 sub 0 sub 0 sub 0 todo" (getField @"todo"
                                                    . getSection 0
                                                    . getSection 0
                                                    . getSection 0
                                                    . getSection 1)
        (Just $ toWord "DONE")
       :> Val "section 1 sub 0 sub 0 sub 0 priority" (getField @"priority"
                                                      . getSection 0
                                                      . getSection 0
                                                      . getSection 0
                                                      . getSection 1)
        Nothing
       :> Val "section 1 sub 0 sub 0 sub 0 body" (getField @"body"
                                                  . getSection 0
                                                  . getSection 0
                                                  . getSection 0
                                                  . getSection 1)
        (DV.fromList [])
       :> Val "section 1 sub 0 sub 0 sub 0 num subsections"
        (F.length . DV.toList
         . getField @"sections"
         . getSection 0
         . getSection 0
         . getSection 0
         . getSection 1) 0

       :> Val "section 2 header" (getField @"header" . getSection 2)
        (toLine "Done third section")
       :> Val "section 2 level" (asInt . getField @"level" . getSection 2) 1
       :> Val "section 2 body" (getField @"body" . getSection 2)
        (DV.fromList [])
       :> Val "num section 2 subsections" (F.length . DV.toList
                                           . getField @"sections"
                                           . getSection 2) 1
       :> Val "section 2 sub 0 header" (getField @"header"
                                                    . getSection 0
                                                    . getSection 2)
        (toLine "Todo sub to third")
       :> Val "section 2 sub 0 todo" (getField @"todo"
                                                    . getSection 0
                                                    . getSection 2)
        Nothing
       :> Val "section 2 sub 0 body" (getField @"body"
                                                  . getSection 0
                                                  . getSection 2)
        (DV.fromList [])
       :> Val "section 2 sub 0 num subsections"
        (F.length . DV.toList
         . getField @"sections"
         . getSection 0
         . getSection 2) 0
      )

  ----------------------------------------------------------------------

  describe "lists" $ do

    verifyParse "Org20" "single list item, dashed"
      (fromString $ unlines [ "- list entry"
                            ])
      (Empty
       :> Val "main body" getBody
        [ toList' OrgBody_dashList [ ([ "list entry" ], []) ]
        ]
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verifyParse "Org21" "multiple list items, dashed"
      (fromString $ unlines [ "- list entry"
                            , "- second list entry"
                            ])
      (Empty
       :> Val "main body" getBody
        [ toList' OrgBody_dashList [ ([ "list entry" ], [])
                                   , ([ "second list entry" ], [])
                                   ]
        ]
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verifyParse "Org22" "many list items, dashed and spaced out"
      (fromString $ unlines [ "- list entry"
                            , "- second list entry"
                            , ""
                            , "-    third list item"
                            , "- fourth item"
                            ])
      (Empty
       :> Val "main body" getBody
        [ toList' OrgBody_dashList [ ([ "list entry" ], [])
                                   , ([ "second list entry" ], [])
                                   , ([ "third list item" ], [])
                                   , ([ "fourth item" ], [])
                                   ]
        ]
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verify "Org23" "many list items, multiple lists" $
      parseTest
      (fromString $ unlines [ "- list entry"
                            , "- second list entry"
                            , ""
                            , ""
                            , "-    third list item"
                            , "- fourth item"
                            ])
      (Empty
       :> Val "main body" getBody
        [ toList' OrgBody_dashList [ ([ "list entry" ], [])
                                   , ([ "second list entry" ], [])
                                   ]
        , toList' OrgBody_dashList [ ([ "third list item" ], [])
                                   , ([ "fourth item" ], [])
                                   ]
        ]
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verifyParse "Org24" "multiple ordered lists"
      (fromString $ unlines [ "1. list entry"
                            , "2. second list entry"
                            , ""
                            , ""
                            , "33.    third list item"
                            , "1) fourth item"
                            , "   1) sub-fourth"
                            , "   1. sub-fourth #2"
                            , "   1)     sub-fourth #3"
                            ])
      (Empty
       :> Val "main body" getBody
        [ toList' OrgBody_enumList [ ([ "list entry" ], [])
                                   , ([ "second list entry" ], [])
                                   ]
        , toList' OrgBody_enumList [ ([ "third list item" ], [])
                                   , ([ "fourth item" ],
                                      [ toList' OrgBody_enumList
                                        [ (["sub-fourth"], [])
                                        , (["sub-fourth #2"], [])
                                        , (["sub-fourth #3"], [])
                                        ]
                                      ])
                                   ]
        ]
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verifyParse "Org25" "many list items, multiple list types"
      (fromString $ unlines [ "- list entry"
                            , "- second list entry"
                            , ""
                            , ""
                            , "-    third list item"
                            , "+ second list"
                            , ""
                            , "+ - second list - item 2"
                            , " * + - third list"
                            , "- last list"
                            ])
      (Empty
       :> Val "main body" getBody
        [ toList' OrgBody_dashList [ ([ "list entry" ], [])
                                   , ([ "second list entry" ], [])
                                   ]
        , toList' OrgBody_dashList [ ([ "third list item" ], []) ]
        , toList' OrgBody_plusList [ ([ "second list"], [])
                                   , ([ "- second list - item 2" ],
                                      [
                                        toList' OrgBody_splatList
                                        [ ([ "+ - third list" ], []) ]
                    ])
                 ]
        , toList' OrgBody_dashList [ ([ "last list" ], []) ]
        ]
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verifyParse "Org26" "sub-lists"
      (fromString $ unlines [ "- list entry"
                            , "- second list entry"
                            , ""
                            , "  -    first sub-list item"
                            , "-      third list item"
                            , "  + l1i3sl1i1"
                            , "  + l1i3sl1i2"
                            , "- fourth item"
                            ])
      (Empty
       :> Val "main body" getBody
        [ toList' OrgBody_dashList [ ([ "list entry" ], [])
                                   , ([ "second list entry" ],
                                      [
                                        toList' OrgBody_dashList
                                        [ ([ "first sub-list item"], []) ]
                                      ])
                                   , ([ "third list item" ],
                                      [
                                        toList' OrgBody_plusList
                                        [ ([ "l1i3sl1i1" ], [])
                                        , ([ "l1i3sl1i2" ], [])
                                        ]
                                      ])
                                   , ([ "fourth item" ], [])
                                   ]
        ]
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verifyParse "Org27" "paras and sub-lists"
      (fromString $ unlines [ "- list entry"
                            , "  This is the first list entry."
                            , "  It has a paragraph."
                            , "- second list entry"
                            , ""
                            , "  There is a paragraph here, too!"
                            , ""
                            , "  More than one, in fact."
                            , "  There are two.  And now there will be"
                            , "  a sublist:"
                            , "  -    first sub-list item"
                            , "-      third list item"
                            , "- fourth item"
                            , ""
                            , "  I4P1"
                            , ""
                            , "  + I4SL1I1 :: item 4, sublist 1, item 1"
                            , "  + I4 SL1   I2 :: ..., item 2"
                            , "  - I4SL2I1"
                            , "  * I4SL3I1"
                            , "    * I4SL3SL1I1 ::not a description"
                            , "      *Fantastic*"
                            , "    + I4SL3SL2I1:: not a description"
                            , "  + I4SL4I1"
                            , "  ++ I4P2"
                            , "  -- I4P3"
                            , "  ** I4P4"
                            , "  Enough!"
                            ])
      (Empty
       :> Val "main body" getBody
        [ toList' OrgBody_dashList
          [ ([ "list entry"
             , "This is the first list entry."
             , "It has a paragraph."
             ], [])
          , ([ "second list entry" ],
              [
                toPara [ "There is a paragraph here, too!" ]
              , toPara [ "More than one, in fact."
                       , "There are two.  And now there will be"
                       , "a sublist:"
                       ]
              , toList' OrgBody_dashList [ ([ "first sub-list item"], []) ]
              ])
          , ([ "third list item" ], [])
          , ([ "fourth item" ],
              [
                toPara [ "I4P1" ]
              , toDescList' OrgBody_plusList
                [ ( ("I4SL1I1", [ "item 4, sublist 1, item 1"]), [])
                , ( ("I4 SL1 I2", [ "..., item 2"]), [])
                ]
              , toList' OrgBody_dashList [ ([ "I4SL2I1" ], []) ]
              , toList' OrgBody_splatList
                [ ([ "I4SL3I1" ],
                    [
                      toList' OrgBody_splatList
                      [ ([ "I4SL3SL1I1 ::not a description" ],
                          [
                            toPara [ "*Fantastic*" ]
                          ])
                      ]
                    , toList' OrgBody_plusList
                      [ ([ "I4SL3SL2I1:: not a description" ], []) ]
                    ])
                ]
              , toList' OrgBody_plusList [ ([ "I4SL4I1" ], []) ]
              , toPara [ "++ I4P2", "-- I4P3", "** I4P4", "Enough!" ]
              ])
          ]
        ]
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    let more4 = [ toPara [ "I4P1" ]
                , toList' OrgBody_plusList [ ([ "I4SL1I1" ], [])
                                           , ([ "" ], [])
                                           ]
                , toList' OrgBody_dashList
                  [ ([ "" ],
                      [ toList' OrgBody_plusList
                        [ ([""],
                            [ toList' OrgBody_enumList [ ([""], []) ]
                            ])
                        , ([""], [])
                        ]
                      ]) ]
                , toList' OrgBody_splatList
                  [ ([ "I4SL3I1" ],
                      [
                        toList' OrgBody_splatList [ ([ "I4SL3SL1I1" ],
                                                     [
                                                       toPara [ "*Fantastic*" ]
                                                     ])
                                                  ]
                      , toList' OrgBody_plusList [ ([ "I4SL3SL2I1" ], []) ]
                      ])
                  ]
                , toList' OrgBody_plusList [ ([ "I4SL4I1" ], []) ]
                , toPara [ "++ I4P2", "-- I4P3", "** I4P4", "Enough!" ]
                ]
      in verifyParse "Org28" "paras and empty entry sub-lists"
         (fromString $ unlines [ "- list entry"
                               , "  This is the first list entry."
                               , "  It has a paragraph."
                               , "- second list entry"
                               , ""
                               , "  There is a paragraph here, too!"
                               , ""
                               , "  More than one, in fact."
                               , "  There are two.  And now there will be"
                               , "  a sublist:"
                               , "  -    first sub-list item"
                               , "-      third list item"
                               , "- fourth item"
                               , ""
                               , "  I4P1"
                               , ""
                               , "  + I4SL1I1"
                               , "  + "
                               , "  -"
                               , "    + "
                               , "      99. "
                               , "    + "
                               , "  * I4SL3I1"
                               , "    * I4SL3SL1I1"
                               , "      *Fantastic*"
                               , "    + I4SL3SL2I1"
                               , "  + I4SL4I1"
                               , "  ++ I4P2"
                               , "  -- I4P3"
                               , "  ** I4P4"
                               , "  Enough!"
                               , ""
                               , "++ P1"
                               , "** S1"
                               , "-- S1P1"
                               ])
         (Empty
          :> Val "main body" getBody
           [ toList' OrgBody_dashList
             [ ([ "list entry"
                , "This is the first list entry."
                , "It has a paragraph."
                ], [])
             , ([ "second list entry" ],
                 [
                   toPara [ "There is a paragraph here, too!" ]
                 , toPara [ "More than one, in fact."
                          , "There are two.  And now there will be"
                          , "a sublist:"
                          ]
                 , toList' OrgBody_dashList [ ([ "first sub-list item"], []) ]
                 ])
             , ([ "third list item" ], [])
             , ([ "fourth item" ], more4)
             ]
           , toPara [ "++ P1" ]
           ]
          :> Val "body list elem 3 entry"
           ( fmap (getField @"entry") . getListItem 3 <=< getBodyIdx 0 )
           (Just $ DV.fromList $ fmap toLine [ "fourth item" :: String ])
          :> Val "body list elem 3 more"
           (fmap (getField @"more") . getListItem 3 <=< getBodyIdx 0 )
           (Just $ DV.fromList more4)
          :> Val "num sections" (F.length . DV.toList . getField @"sections") 1
          :> Val "section 0 header" (getField @"header" . getSection 0)
           (toLine "")
          :> Val "section 0 body" (getField @"body" . getSection 0)
           (DV.fromList [])
          :> Val "section 0 num subsections" (F.length . DV.toList
                                             . getField @"sections"
                                             . getSection 0) 1
          :> Val "section 0 sub 0 header"
           (getField @"header" . getSection 0 . getSection 0)
           (toLine "S1")
          :> Val "section 0 sub 0 body"
           (getField @"body" . getSection 0 . getSection 0)
           (DV.fromList [ toPara [ "-- S1P1"] ])
          :> Val "section 0 sub 0 num subsections"
           (F.length . DV.toList . getField @"sections"
            . getSection 0
            . getSection 0) 0
         )

    verifyParse "Org29" "single description list item, dashed"
      (fromString $ unlines [ "- list  ::  entry"
                            ])
      (Empty
       :> Val "main body" getBody
        [ toDescList' OrgBody_dashList [ (("list", [ "entry" ]), []) ]
        ]
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verifyParse "Org2a" "single description list item, plused, no desc"
      (fromString $ unlines [ "- list  entry ::"
                            ])
      (Empty
       :> Val "main body" getBody
        [ toDescList' OrgBody_dashList [ (("list entry", [""]), []) ]
        ]
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verifyParse "Org2b" "single description list item, enum, no term"
      (fromString $ unlines [ "123) ::  mo better"
                            ])
      (Empty
       :> Val "main body" getBody
        [ toDescList' OrgBody_enumList [ (("", ["mo better"]), []) ]
        ]
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verifyParse "Org2c" "no description list, second line"
      (fromString $ unlines [ "- I hope"
                            , "  you feel :: mo better"
                            ])
      (Empty
       :> Val "main body" getBody
        [toList' OrgBody_dashList [ (["I hope", "you feel :: mo better"], [])]
        ]
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verifyParse "Org2d" "two blank lines end all lists"
      (fromString $ unlines [ "- I hope"
                            , "  * you feel"
                            , "    + mo better"
                            , ""
                            , ""
                            , "- This is a different list"
                            ])
      (Empty
       :> Val "main body" getBody
        [toList' OrgBody_dashList
          [ ( ["I hope"]
            , [ toList' OrgBody_splatList
                [ ( ["you feel" ]
                  , [ toList' OrgBody_plusList [ (["mo better"], []) ] ])
                ]
              ])
          ]
        , toList' OrgBody_dashList [ ( ["This is a different list"], []) ]
        ]
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verifyParse "Org2e" "checkboxes"
      (fromString $ unlines [ "- [ ] first :: here"
                            , "- [-] second"
                            , "- [X] third"
                            , "- [y] no checkbox fourth"
                            , "- [ ]no checkbox fifth"
                            , "- no [ ] checkbox sixth"
                            ])
      (Empty
       :> Val "main body" getBody
       (let noMore = DV.fromList []
            cb = Just . lit . toInteger . ord
        in [ OrgBody_dashList $ DV.fromList
             [ ListItem (cb ' ')
               (Just $ toLine "first") (DV.fromList [toLine "here"]) noMore
             , ListItem (cb '-') Nothing (DV.fromList [toLine "second"]) noMore
             , ListItem (cb 'X') Nothing (DV.fromList [toLine "third"]) noMore
             , ListItem Nothing Nothing
               (DV.fromList [toLine "[y] no checkbox fourth"]) noMore
             , ListItem Nothing Nothing
               (DV.fromList [toLine "[ ]no checkbox fifth"]) noMore
             , ListItem Nothing Nothing
               (DV.fromList [toLine "no [ ] checkbox sixth"]) noMore
             ]
           ]
       )
      )

  ----------------------------------------------------------------------

  describe "drawers" $ do

    verifyParse "Org30" "empty drawer"
      (fromString $ unlines [ ":foo_drawer:", ":end:" ])
      (Empty
       :> Got "main body" (isJust . (getDrawer <=< getBodyIdx 0))
       :> Val "main body drawer name"
        (fmap (getField @"name") . (getDrawer <=< getBodyIdx 0))
        (Just $ toWord "foo_drawer")
       :> Val "main body drawer contents"
        (fmap (getField @"contents") . (getDrawer <=< getBodyIdx 0))
        (Just $ DV.fromList [])
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verifyParse "Org31" "para and empty drawer"
      (fromString $ unlines [ "This is a paragraph.", ":foo_drawer:", ":end:" ])
      (Empty
       :> Val "main body length" (F.length . getBody) 2
       :> Val "main body paragraph" (getBodyIdx 0)
        (Just $ toPara [ "This is a paragraph." ])
       :> Val "main body drawer name"
        (fmap (getField @"name") . (getDrawer <=< getBodyIdx 1))
        (Just $ toWord "foo_drawer")
       :> Val "main body drawer contents"
        (fmap (getField @"contents") . (getDrawer <=< getBodyIdx 1))
        (Just $ DV.fromList [])
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verifyParse "Org32" "empty drawer"
      (fromString $ unlines [ ":foo_drawer:", ":end:", "End paragraph" ])
      (Empty
       :> Got "main body" (isJust . (getDrawer <=< getBodyIdx 0))
       :> Val "main body length" (F.length . getBody) 2
       :> Val "main body drawer name"
        (fmap (getField @"name") . (getDrawer <=< getBodyIdx 0))
        (Just $ toWord "foo_drawer")
       :> Val "main body drawer contents"
        (fmap (getField @"contents") . (getDrawer <=< getBodyIdx 0))
        (Just $ DV.fromList [])
       :> Val "main body paragraph" (getBodyIdx 1)
        (Just $ toPara [ "End paragraph" ])
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verifyParse "Org33" "drawer with paragraphs and lists"
      (fromString $ unlines [ "This is the start"
                            , ":foo_drawer:"
                            , "Drawer paragraph"
                            , "is here"
                            , ":no_sub_drawer:"
                            , "- with a list"
                            , "  + and a sublist"
                            , ":And another paragraph"
                            , ""
                            , ""
                            , ""
                            , ":"
                            , ":e"
                            , ":ee"
                            , ":en"
                            , ":en:"
                            , ":ena"
                            , ":end"
                            , ":ende:"
                            , "an :end:"
                            , "  :end:"
                            , "And this is the end"
                            ])
      (Empty
       :> Val "main body length" (F.length . getBody) 3
       :> Val "main body paragraph" (getBodyIdx 0)
        (Just $ toPara [ "This is the start" ])
       :> Got "main body" (isJust . (getDrawer <=< getBodyIdx 1))
       :> Val "main body drawer name"
        (fmap (getField @"name") . (getDrawer <=< getBodyIdx 1))
        (Just $ toWord "foo_drawer")
       :> Val "main body drawer contents"
        (fmap (getField @"contents") . (getDrawer <=< getBodyIdx 1))
        (Just $ DV.fromList
         [
           toPara [ "Drawer paragraph", "is here", ":no_sub_drawer:" ]
         , toList' OrgBody_dashList
           [ ( ["with a list"],
               [ toList' OrgBody_plusList [ (["and a sublist"], []) ] ]) ]
         , toPara [ ":And another paragraph" ]
         , toPara [ ":", ":e", ":ee", ":en", ":en:", ":ena", ":end"
                  , ":ende:", "an :end:" ]
         ])
       :> Val "main body end paragraph" (getBodyIdx 2)
        (Just $ toPara [ "And this is the end" ])
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

  ----------------------------------------------------------------------

    verifyParse "Org70" "solo settings line"
      (fromString $ unlines [ "#+FILETAGS:" ])
      (Empty
       :> Got "main body setting" (isJust . (getSetting <=< getBodyIdx 0))
       :> Val "main body length" (F.length . getBody) 1
       :> Val "main body setting keyword"
        (fmap (getField @"keyword") . (getSetting <=< getBodyIdx 0))
        (Just $ toWord "FILETAGS")
       :> Val "main body setting values"
        (fmap (getField @"values") . (getSetting <=< getBodyIdx 0))
        (Just $ DV.fromList [])
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verifyParse "Org71" "empty settings line"
      (fromString $ unlines [ "#+FILETAGS:", "" ])
      (Empty
       :> Got "main body setting" (isJust . (getSetting <=< getBodyIdx 0))
       :> Val "main body length" (F.length . getBody) 1
       :> Val "main body setting keyword"
        (fmap (getField @"keyword") . (getSetting <=< getBodyIdx 0))
        (Just $ toWord "FILETAGS")
       :> Val "main body setting values"
        (fmap (getField @"values") . (getSetting <=< getBodyIdx 0))
        (Just $ DV.fromList [])
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verifyParse "Org72" "no settings line"
      (fromString $ unlines [ "#+FILETAGS: ", "" ])
      (Empty
       :> Got "main body setting" (isJust . (getSetting <=< getBodyIdx 0))
       :> Val "main body length" (F.length . getBody) 1
       :> Val "main body setting keyword"
        (fmap (getField @"keyword") . (getSetting <=< getBodyIdx 0))
        (Just $ toWord "FILETAGS")
       :> Val "main body setting values"
        (fmap (getField @"values") . (getSetting <=< getBodyIdx 0))
        (Just $ DV.fromList [])
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verifyParse "Org73" "no settings line"
      (fromString $ unlines [ "#+FILETAGS: t1 :t2 t:3"
                            , "#+CATEGORY: c"
                            , "#+wish: true"
                            , "hi"
                            , "#+foo: bar"
                            , "#+Archive: %s_done::"
                            ])
      (Empty
       :> Got "main body setting" (isJust . (getSetting <=< getBodyIdx 0))
       :> Val "main body length" (F.length . getBody) 4

       :> Val "main body setting 0 keyword"
        (fmap (getField @"keyword") . (getSetting <=< getBodyIdx 0))
        (Just $ toWord "FILETAGS")
       :> Val "main body setting 0 values"
        (fmap (getField @"values") . (getSetting <=< getBodyIdx 0))
        (Just $ DV.fromList [ toWord "t1", toWord ":t2", toWord "t:3" ])

       :> Val "main body 1 setting keyword"
        (fmap (getField @"keyword") . (getSetting <=< getBodyIdx 1))
        (Just $ toWord "CATEGORY")
       :> Val "main body 1 setting values"
        (fmap (getField @"values") . (getSetting <=< getBodyIdx 1))
        (Just $ DV.fromList [ toWord "c" ])

       :> Val "main body 2 para" (getPara <=< getBodyIdx 2)
        (Just $ DV.fromList (fmap toLine [ "#+wish: true"
                                         , "hi"
                                         , "#+foo: bar"
                                         ]))

       :> Val "main body 3 setting keyword"
        (fmap (getField @"keyword") . (getSetting <=< getBodyIdx 3))
        (Just $ toWord "Archive")
       :> Val "main body 3 setting values"
        (fmap (getField @"values") . (getSetting <=< getBodyIdx 3))
        (Just $ DV.fromList [ toWord "%s_done::" ])

       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

  ----------------------------------------------------------------------

  describe "blocks" $ do

    verifyParse "Org40" "empty block"
      (fromString $ unlines [ "#+BEGIN_foo", "#+END_foo" ])
      (Empty
       :> Got "main body is block" (isJust . (getBlock <=< getBodyIdx 0))
       :> Val "main body length" (F.length . getBody) 1
       :> Val "main body block type"
        (fmap (getField @"type") . (getBlock <=< getBodyIdx 0))
        (Just $ toWord "foo")
       :> Val "main body block contents"
        (fmap (getField @"contents") . (getBlock <=< getBodyIdx 0))
        (Just $ DV.fromList [])
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verifyParse "Org41" "block with data"
      (fromString $ unlines [ "#+BEGIN_yowza"
                            , "block body"
                            , "- anything goes"
                            , ":totally:"
                            , "1) uninterpreted"
                            , ":end:"
                            , ",#+END_foo"
                            , "#+END_yowza" ])
      (Empty
       :> Got "main body is block" (isJust . (getBlock <=< getBodyIdx 0))
       :> Val "main body length" (F.length . getBody) 1
       :> Val "main body block type"
        (fmap (getField @"type") . (getBlock <=< getBodyIdx 0))
        (Just $ toWord "yowza")
       :> Val "main body block contents"
        (fmap (getField @"contents") . (getBlock <=< getBodyIdx 0))
        (Just $ DV.fromList $ fmap toWord
         [ "block body"
         , "- anything goes"
         , ":totally:"
         , "1) uninterpreted"
         , ":end:"
         , ",#+END_foo"
         ])
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verifyParse "Org42" "unclosed block is just a para"
      (fromString $ unlines [ "#+BEGIN_foo", "stuff", "#+END_fool" ])
      (Empty
       :> Val "main body length" (F.length . getBody) 1
       :> Val "main body" getBody
        [ toPara [ "#+BEGIN_foo", "stuff", "#+END_fool" ] ]
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verifyParse "Org43" "body with blocks"
      (fromString $ unlines [ "Intro"
                            , "#+BEGIN_example"
                            , "block body"
                            , "#+end_example"
                            , "- list item 1"
                            , "  #+BEGINNER_stuff"
                            , "  + [X] sublist :: item 1"
                            , "    #+begin_src c -n 29 -r -l \"((%s))\""
                            , "    - just C code"
                            , "int main(int argc, char **argv)"
                            , "{ printf(\"Hello, World\\n\"); }"
                            , "#+END_src"
                            , "  + sublist item 2"
                            ])
      (Empty
       :> Val "main body length" (F.length . getBody) 3
       :> Val "main body 0 para" (getBodyIdx 0)
        (Just $ toPara [ "Intro" ])
       :> Val "main body 1 block type"
        (fmap (getField @"type") . (getBlock <=< getBodyIdx 1))
        (Just $ toWord "example")
       :> Val "main body 1 block args"
        (fmap (getField @"args") . (getBlock <=< getBodyIdx 1))
        (Just $ DV.fromList [])
       :> Val "main body block contents"
        (fmap (getField @"contents") . (getBlock <=< getBodyIdx 1))
        (Just $ DV.fromList $ fmap toWord [ "block body"])
       :> Val "main body 2 list item 0"
        (fmap (getField @"entry") . (getListItem 0 <=< getBodyIdx 2))
        (Just $ DV.fromList $ fmap toLine [ "list item 1", "#+BEGINNER_stuff" ])
       :> Val "main body 2 list item 0 sub 0 term" ( getField @"term"
                                                     <=< getListItem 0
                                                     <=< atVIdx 0
                                                     <=< fmap (getField @"more")
                                                     . getListItem 0
                                                     <=< getBodyIdx 2)
        (Just $ DV.fromList $ fmap toWord [ "sublist" ])
       :> Val "main body 2 list item 0 sub 0 entry" ( fmap (getField @"entry")
                                                      .  getListItem 0
                                                      <=< atVIdx 0
                                                      <=< fmap (getField @"more")
                                                      . getListItem 0
                                                      <=< getBodyIdx 2)
        (Just $ DV.fromList $ fmap toLine [ "item 1" ])
       :> Val "main body 2 list item 0 suv 0 checkbox" ( fmap (getField @"checkbox")
                                                         . getListItem 0
                                                         <=< atVIdx 0
                                                         <=< fmap (getField @"more")
                                                         . getListItem 0
                                                         <=< getBodyIdx 2)
        (Just $ Just $ lit $ toInteger $ ord 'X')
       :> Val "main body 2 list item 0 sub 0 more block type"
        ( fmap (getField @"type")
          . getBlock
          <=< atVIdx 0
          <=< fmap (getField @"more")
          . getListItem 0
          <=< atVIdx 0
          <=< fmap (getField @"more")
          . getListItem 0
          <=< getBodyIdx 2)
        (Just $ toWord "src")
       :> Val "main body 2 list item 0 sub 0 more block args"
        ( fmap (getField @"args")
          . getBlock
          <=< atVIdx 0
          <=< fmap (getField @"more")
          . getListItem 0
          <=< atVIdx 0
          <=< fmap (getField @"more")
          . getListItem 0
          <=< getBodyIdx 2)
        (Just $ DV.fromList $ fmap toWord [ "c", "-n", "29", "-r"
                                          , "-l", "\"((%s))\""
                                          ])
       :> Val "main body 2 list item 0 more block contents"
        ( fmap (getField @"contents")
          . getBlock
          <=< atVIdx 0
          <=< fmap (getField @"more")
          . getListItem 0
          <=< atVIdx 0
          <=< fmap (getField @"more")
          . getListItem 0
          <=< getBodyIdx 2)
        (Just $ DV.fromList $ fmap toWord [ "    - just C code"
                                          , "int main(int argc, char **argv)"
                                          , "{ printf(\"Hello, World\\n\"); }"
                                          ])

       :> Val "main body 2 list item 0 sub 1 term" ( getField @"term"
                                                     <=< getListItem 1
                                                     <=< atVIdx 0
                                                     <=< fmap (getField @"more")
                                                     . getListItem 0
                                                     <=< getBodyIdx 2)
        Nothing
       :> Val "main body 2 list item 0 sub 0 entry" ( fmap (getField @"entry")
                                                      .  getListItem 1
                                                      <=< atVIdx 0
                                                      <=< fmap (getField @"more")
                                                      . getListItem 0
                                                      <=< getBodyIdx 2)
        (Just $ DV.fromList $ fmap toLine [ "sublist item 2" ])
       :> Val "main body 2 list item 0 suv 0 checkbox" ( fmap (getField @"checkbox")
                                                         . getListItem 1
                                                         <=< atVIdx 0
                                                         <=< fmap (getField @"more")
                                                         . getListItem 0
                                                         <=< getBodyIdx 2)
        (Just Nothing)
       :> Val "main body 2 list item 0 sub 0 more block type"
        ( fmap (getField @"more")
          . getListItem 1
          <=< atVIdx 0
          <=< fmap (getField @"more")
          . getListItem 0
          <=< getBodyIdx 2)
        (Just $ DV.fromList [])

       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

  ----------------------------------------------------------------------

  describe "markup" $ do

    verify "Org50" "markup" $
      parseTest
      (fromString $ unlines [ "Hello, world", "Hiya *to _you_* /back/!" ])
      (Empty
       :> Val "main body" (getPara <=< getBodyIdx 0)
        (Just $ DV.fromList (fmap toLine [ "Hello, world"
                                         , "Hiya *to _you_* /back/!"
                                         ]))
       :> Val "main body with markup"
        (fmap orgMarkupParse . getPara <=< getBodyIdx 0)
        (Just [ OrgText_text [["Hello,", "world"], ["Hiya"]]
              , OrgText_bold [ OrgText_text [["to"]]
                             , OrgText_underline [OrgText_text [["you"]]]
                             ]
              , OrgText_italics [ OrgText_text [["back"]] ]
              , OrgText_adj [["!"]]
              ])
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verify "Org51" "non-markup" $
      parseTest
      (fromString $ unlines [ "Hello,_ _ world_"
                            , "Hiya *to just _you there* /back/!" ])
      (Empty
       :> Val "main body" (getPara <=< getBodyIdx 0)
        (Just $ DV.fromList (fmap toLine [ "Hello,_ _ world_"
                                         , "Hiya *to just _you there* /back/!"
                                         ]))
       :> Val "main body with markup"
        (fmap orgMarkupParse . getPara <=< getBodyIdx 0)
        (Just [ OrgText_text [["Hello,_", "_", "world_"], ["Hiya"]]
              , OrgText_bold [ OrgText_text [["to", "just", "_you", "there"]] ]
              , OrgText_italics [ OrgText_text [["back"]] ]
              , OrgText_adj [["!"]]
              ])
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verify "Org52" "immediate markup" $
      parseTest
      (fromString $ unlines [ "_Hello,_ world_" ])
      (Empty
       :> Val "main body" (getPara <=< getBodyIdx 0)
        (Just $ DV.fromList (fmap toLine [ "_Hello,_ world_" ]))
       :> Val "main body with markup"
        (fmap orgMarkupParse . getPara <=< getBodyIdx 0)
        (Just [ OrgText_underline [ OrgText_text [["Hello,"]] ]
              , OrgText_text [["world_"]]
              ])
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verify "Org53" "nested markup including punct " $
      parseTest
      (fromString $ unlines [ "*_Hello_,* world_" ])
      (Empty
       :> Val "main body" (getPara <=< getBodyIdx 0)
        (Just $ DV.fromList (fmap toLine [ "*_Hello_,* world_" ]))
       :> Val "main body with markup"
        (fmap orgMarkupParse . getPara <=< getBodyIdx 0)
        (Just [ OrgText_bold [ OrgText_underline [ OrgText_text [["Hello"]] ]
                             , OrgText_adj [[","]]]
              , OrgText_text [["world_"]]
              ])
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verify "Org53" "link markup no desc " $
      parseTest
      (fromString $ unlines [ "I am going to give\nyou a [[link]] to use." ])
      (Empty
       :> Val "main body" (getPara <=< getBodyIdx 0)
        (Just $ DV.fromList (fmap toLine [ "I am going to give"
                                         , "you a [[link]] to use." ]))
       :> Val "main body with markup"
        (fmap orgMarkupParse . getPara <=< getBodyIdx 0)
        (Just [ OrgText_text [ ["I", "am", "going", "to", "give"]
                             , ["you", "a"]]
              , OrgText_link (OrgLink "link" Nothing)
              , OrgText_text [["to", "use."]]
              ])
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verify "Org54" "link markup with desc and target and radio target" $
      parseTest
      (fromString "I <<<am /going/ to>>> give\nyou a [[a_link][/hopefully/ useful Link]] to <<use>>; here.")
      (Empty
       :> Val "main body" (getPara <=< getBodyIdx 0)
        (Just $ DV.fromList
         (fmap toLine
           [ "I <<<am /going/ to>>> give"
           , "you a [[a_link][/hopefully/ useful Link]] to <<use>>; here." ]))
       :> Val "main body with markup"
        (fmap orgMarkupParse . getPara <=< getBodyIdx 0)
        (Just [ OrgText_text [ ["I"] ]
              , OrgText_radio_target [ OrgText_text [["am"]]
                                     , OrgText_italics [OrgText_text [["going"]]]
                                     , OrgText_text [["to"]]]
              , OrgText_text [ [ "give"]
                             , ["you", "a"]]
              , OrgText_link
                (OrgLink "a_link"
                 (Just [OrgText_italics [OrgText_text [["hopefully"]]]
                       , OrgText_text [["useful", "Link"]]]))
              , OrgText_text [["to"]]
              , OrgText_link_target "use"
              , OrgText_adj [[";", "here."]]
              ])
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verify "Org55" "unclosed link markup with desc " $
      parseTest
      (fromString "I am not going to give\nyou a [[a_link][/hopefully/ useful Link] to use.")
      (Empty
       :> Val "main body" (getPara <=< getBodyIdx 0)
        (Just $ DV.fromList
         (fmap toLine
           [ "I am not going to give"
           , "you a [[a_link][/hopefully/ useful Link] to use." ]))
       :> Val "main body with markup"
        (fmap orgMarkupParse . getPara <=< getBodyIdx 0)
        (Just [ OrgText_text [ ["I", "am", "not", "going", "to", "give"]
                             , ["you", "a"
                               , "[[a_link][/hopefully/", "useful"
                               , "Link]", "to", "use."]
                             ]
              ])
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

    verify "Org56" "link markup with one-word desc" $
      parseTest
      (fromString "A [[a_link][Link]] here.")
      (Empty
       :> Val "main body" (getPara <=< getBodyIdx 0)
        (Just $ DV.fromList
         (fmap toLine [ "A [[a_link][Link]] here." ]))
       :> Val "main body with markup"
        (fmap orgMarkupParse . getPara <=< getBodyIdx 0)
        (Just
         [ OrgText_text [ ["A"] ]
         , OrgText_link $ OrgLink "a_link" $ Just [ OrgText_text [["Link"]] ]
         , OrgText_text [["here."]]
         ])
       :> Val "num sections" (F.length . DV.toList . getField @"sections") 0
      )

toWord :: String ->  DV.Vector (UInt 8)
toWord = DV.fromList . fmap (lit . toInteger . ord)  -- aka. DV.stringToVec

toLine :: String -> DV.Vector (DV.Vector (UInt 8))
toLine =  DV.fromList . fmap toWord . words

toPara :: [String] -> OrgBody
toPara = OrgBody_paragraph . DV.fromList . fmap toLine

toList' :: (DV.Vector ListItem -> OrgBody) -> [ ([String], [OrgBody]) ] -> OrgBody
toList' lCons =
  let toListItem (entry, more) = ListItem Nothing Nothing
                                 (DV.fromList $ fmap toLine entry)
                                 (DV.fromList more)
  in lCons . DV.fromList . fmap toListItem

toDescList' :: (DV.Vector ListItem -> OrgBody) -> [ ((String, [String]), [OrgBody]) ] -> OrgBody
toDescList' lCons =
  let toListItem ((term, entry), more) =
        ListItem
        Nothing
        (Just $ toLine term)
        (DV.fromList $ fmap toLine entry)
        (DV.fromList more)
  in lCons . DV.fromList . fmap toListItem

getBody :: HasField "body" r (DV.Vector a) => DV.VecElem a => r -> [a]
getBody = DV.toList . getField @"body"

getBodyIdx :: HasField "body" r (DV.Vector a) => DV.VecElem a => Int -> r -> Maybe a
getBodyIdx n = atVIdx n . getField @"body"

getPara :: OrgBody -> Maybe OrgPara
getPara = \case
  OrgBody_paragraph p -> Just p
  _ -> Nothing

getListItem :: Int -> OrgBody -> Maybe ListItem
getListItem n = \case
  OrgBody_dashList l -> atVIdx n l
  OrgBody_plusList l -> atVIdx n l
  OrgBody_enumList l -> atVIdx n l
  OrgBody_splatList l -> atVIdx n l
  OrgBody_drawer {} -> Nothing
  OrgBody_aBlock {} -> Nothing
  OrgBody_setting {} -> Nothing
  OrgBody_paragraph _ -> Nothing

getDrawer :: OrgBody -> Maybe Drawer
getDrawer = \case
  OrgBody_drawer d -> pure d
  _ -> Nothing

getBlock :: OrgBody -> Maybe Block
getBlock = \case
  OrgBody_aBlock d -> pure d
  _ -> Nothing

getSetting :: OrgBody -> Maybe Setting
getSetting = \case
  OrgBody_setting s -> pure s
  _ -> Nothing

getSection :: HasField "sections" r (DV.Vector OrgSection)
           => DV.VecElem OrgSection
           => Int -> r -> OrgSection
getSection n = let invIdx = error $ "<invalid section index " <> show n <> ">"
               in maybe invIdx id . atVIdx n . getField @"sections"
