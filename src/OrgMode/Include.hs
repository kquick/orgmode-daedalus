{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module OrgMode.Include
  (
    includeOrgInSource
  )
where

import           Data.Name
import           Data.String ( fromString )
import qualified Data.Text.IO as TIO
import           Language.Haskell.TH hiding ( Name )
import           System.Exit
import           System.IO
import           Text.Sayable

import           OrgMode.Parse ( orgStructureParse )


-- | The includeOrgInSource can be used to ingest an org-mode file at compile
-- time and store the result in the specified variable.  This can be useful to
-- include help information or other documentation in an executable while
-- ensuring the inclusion is valid and precluding run-time errors.

includeOrgInSource :: FilePath -- ^ Path to the org-mode file
                   -> String -- ^ Haskell variable to hold the parsed OrgDoc
                   -> Q [Dec]
includeOrgInSource fp n = do
  b <- fromText @(Name "input text") <$> (runIO $ TIO.readFile fp)
  let fn = fromString @(Name "input name") fp
  case orgStructureParse fn b (fromString "Invalid org file provided") of
    Right _ -> do
      -- There is no Lift instance for OrgDoc, nor can one be defined here
      -- (constructors are not available), so the following cannot be used:
      --
      -- pExp <- runQ [| p |]
      -- return
      --   [ SigD (mkName n) (ConT $ mkName "OrgDoc")
      --   , ValD (VarP (mkName n)) (NormalB pExp) []
      --   ]
      --
      -- However, we know at this point that it will parse correctly, so we can
      -- parse the contents at runtime to reconstruct the needed value.
      nExp <- [| case orgStructureParse (fromString fp) b
                           (fromString "Invalid org file provided") of
                       Right p -> p
                       Left err -> error $ sez @"error" err -- never used due to test above
              |]
      return
        [ SigD (mkName n) (ConT $ mkName "OrgDoc")
        , FunD (mkName n) [Clause [] (NormalB nExp) []]
        ]
    Left err -> runIO $
                do hPutStrLn stderr $ sez @"error" $ "Org parse failed:" &- err
                   exitFailure
