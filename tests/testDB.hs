{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe
import Data.Time.Zones.DB
import Data.Time.Zones.Files
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit hiding (Test, assert)
import System.Posix.Env

case_Budapest_is_Budapest :: IO ()
case_Budapest_is_Budapest = do
  pathBp <- timeZonePathFromDB "Europe/Budapest"
  readBp <- BL.readFile pathBp
  readBp @=? tzDataByLabel Europe__Budapest
  let Just budByName = tzDataByName "Europe/Budapest"
  readBp @=? budByName

case_Etc__GMT1_is_Etc__GMT1 :: IO ()
case_Etc__GMT1_is_Etc__GMT1 = do
  pathEtc <- timeZonePathFromDB "Etc/GMT+1"
  readEtc <- BL.readFile pathEtc
  readEtc @=? tzDataByLabel Etc__GMT1
  let Just etcByName = tzDataByName "Etc/GMT+1"
  readEtc @=? etcByName

case_aliases :: IO ()
case_aliases =
  America__Argentina__Buenos_Aires @=? fromJust (fromTZName "America/Buenos_Aires")

case_fromToName :: IO ()
case_fromToName = forM_ [minBound .. maxBound] t
  where
    t :: TZLabel -> IO ()
    t label = Just label @=? fromTZName (toTZName label)

main :: IO ()
main = do
  -- When we are running 'cabal test' the package is not yet
  -- installed, so we want to use the data directory from within the
  -- sources.
  setEnv "tz_datadir" "./tzdata" True
  $defaultMainGenerator
