{-# LANGUAGE TemplateHaskell #-}

module Test.Scenarios.ShelleyAddressBuild
  ( tests
  ) where

import           Cardano.Prelude

import           Hedgehog (Property, discover)
import qualified Hedgehog as H
import           System.Directory (doesFileExist)

import           Test.Scenarios.OptParse


-- | This property checks the scenario where we would like to
-- build a shelley address.
-- 1. We generate a key pair
-- 2. We check for the existence of the key pair
-- 3. We use the generated verification key to build a shelley address.
prop_shelleyAddressBuild :: Property
prop_shelleyAddressBuild =
  H.withTests 1 . H.property $ do

    -- Key filepaths
    let addrVerKey = "hedgehog-addr-v-key"
        addrSignKey = "hedgehog-addr-s-key"

    -- Generate payment verification key
    execCardanoCLIParser
      [addrVerKey, addrSignKey]
      "Failure: prop_shelleyAddressBuild.address_gen"
        $ evalCardanoCLIParser [ "shelley","address","key-gen"
                               , "--verification-key-file", addrVerKey
                               , "--signing-key-file", addrSignKey
                               ]

    -- Check for existence of file. This error can be rendered in a nicer way.
    verificationKeyExists <- liftIO $ doesFileExist addrVerKey
    H.assert verificationKeyExists
    signingKeyExists <- liftIO $ doesFileExist addrSignKey
    H.assert signingKeyExists

    -- Build shelley address
    execCardanoCLIParser
      []
      "Failure: prop_shelleyAddressBuild.build_address"
        $ evalCardanoCLIParser [ "shelley", "address", "build"
                               , "--payment-verification-key-file", addrVerKey
                               ]

    liftIO $ fileCleanup [addrVerKey, addrSignKey]
    H.success


-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
