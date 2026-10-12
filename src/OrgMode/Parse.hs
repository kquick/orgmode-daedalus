{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
-- Daedalus GHC TH output is based on template, so it creates definitions without
-- regard for whether they will be used.  Tell GHC that's OK:
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{- OPTIONS_GHC -ddump-splices #-}


module OrgMode.Parse
  (
    orgStructureParse
  , OrgDoc(..)
  , OrgSection(..)
  , OrgBody(..)
  , ListItem(..)
  , Drawer(..)
  , Block(..)
  , Setting(..)
  )
  where

import Daedalus.Diagnostic
import Daedalus.RTS
import Daedalus.TH.Compile
import Data.Name
import Data.Text ( Text )
import Data.Text.Encoding ( encodeUtf8 )
import Error.Diagnose


compileDDLWith defaultConfig { errorLevel = 2
                             , specPath = [ "ddl" ]
                             } (FromFile "ddl/org_mode.ddl")


-- | This is the main structural parser.
orgStructureParse :: Name "input name" -> Name "input text"
                  -> Name "operation error"
                  -- ^ what is the general operation that failed
                  -> Either (Diagnostic Text) OrgDoc
orgStructureParse inpFrom inptxt opErr =
  let i = newInput "org-mode source" $ encodeUtf8 $ nameText inptxt
  in case runDParser $ pMain i of
       Right r -> Right r
       Left e -> Left $ daedalusDiagnostic opErr inpFrom inptxt e
