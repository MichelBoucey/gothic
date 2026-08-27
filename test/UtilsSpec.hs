{-# LANGUAGE OverloadedStrings #-}

module UtilsSpec (spec) where

import           Test.Hspec

import           Database.Vault.KVv2.Client

spec :: Spec
spec = describe "Utils" $ do

  describe "toSecretData" $ do
    it "converts empty list to SecretData" $
      toSecretData [] `shouldBe` toSecretData []

    it "converts single key-value pair" $
      toSecretData [("key", "val")] `shouldBe` toSecretData [("key", "val")]

    it "converts multiple key-value pairs" $
      toSecretData [("a", "1"), ("b", "2")] `shouldBe` toSecretData [("a", "1"), ("b", "2")]

  describe "fromSecretData" $ do
    it "round-trips with toSecretData for empty list" $
      fromSecretData (toSecretData []) `shouldMatchList` []

    it "round-trips with toSecretData for single pair" $
      fromSecretData (toSecretData [("k", "v")]) `shouldMatchList` [("k", "v")]

    it "round-trips with toSecretData for multiple pairs" $
      fromSecretData (toSecretData [("a", "1"), ("b", "2")])
        `shouldMatchList` [("a", "1"), ("b", "2")]

  describe "toSecretVersions" $ do
    it "converts empty list" $
      toSecretVersions [] `shouldBe` toSecretVersions []

    it "converts single version" $
      toSecretVersions [1] `shouldBe` toSecretVersions [1]

    it "converts multiple versions" $
      toSecretVersions [1, 2, 3] `shouldBe` toSecretVersions [1, 2, 3]
