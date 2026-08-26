{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where

import           Control.Exception                (finally)
import           Data.Either                      (isLeft, isRight)
import           Test.Hspec

import           Database.Vault.KVv2.Client
import           Database.Vault.KVv2.Client.Types

testPaths :: [String]
testPaths =
  [ "test/api/putget"
  , "test/api/versions"
  , "test/api/del"
  , "test/api/delver"
  , "test/api/destroy"
  , "test/api/destver"
  , "test/api/list/"
  , "test/api/list/secret1"
  , "test/api/list/secret2"
  , "test/api/cfg"
  ]

withConn :: (VaultConnection -> IO ()) -> IO ()
withConn action = do
  result <- vaultConnect (Just "http://localhost:8200") "secret" (Just "A_SECRET_TOKEN") True
  case result of
    Right conn -> action conn `finally` cleanup conn
    Left err   -> error $ "Failed to connect to Vault: " ++ err
  where
    cleanup conn =
      mapM_ (\p -> destroySecret conn (SecretPath p) >> return ()) testPaths

getCurrentVersion :: VaultConnection -> String -> IO Int
getCurrentVersion conn p = do
  result <- currentSecretVersion conn (SecretPath p)
  case result of
    Right (SecretVersion v) -> return v
    Left _                  -> return 0

spec :: Spec
spec = aroundAll withConn $ do

  describe "vaultConnect" $ do
    it "connects to Vault and returns VaultConnection" $ \conn ->
      show conn `shouldSatisfy` (not . null)

  describe "putSecret + getSecret" $ do
    it "creates and reads a secret" $ \conn -> do
      let sd = toSecretData [("user", "admin"), ("pass", "secret123")]
      putResult <- putSecret conn WriteAllowed (SecretPath "test/api/putget") sd
      putResult `shouldSatisfy` isRight
      getResult <- getSecret conn (SecretPath "test/api/putget") Nothing
      getResult `shouldBe` Right sd

    it "putSecret with CurrentVersion CAS" $ \conn -> do
      cv <- getCurrentVersion conn "test/api/putget"
      let sd = toSecretData [("foo", "bar")]
      putResult <- putSecret conn (CurrentVersion cv) (SecretPath "test/api/putget") sd
      putResult `shouldSatisfy` isRight

  describe "currentSecretVersion" $ do
    it "returns the current version number" $ \conn -> do
      vResult <- currentSecretVersion conn (SecretPath "test/api/putget")
      vResult `shouldSatisfy` isRight

  describe "readSecretMetadata" $ do
    it "returns version metadata" $ \conn -> do
      mResult <- readSecretMetadata conn (SecretPath "test/api/putget")
      mResult `shouldSatisfy` isRight

  describe "putSecret multiple versions" $ do
    it "increments version on each put" $ \conn -> do
      startV <- getCurrentVersion conn "test/api/versions"
      let sd1 = toSecretData [("v", "one")]
          sd2 = toSecretData [("v", "two")]
          sd3 = toSecretData [("v", "three")]
      r1 <- putSecret conn WriteAllowed (SecretPath "test/api/versions") sd1
      r1 `shouldBe` Right (SecretVersion (startV + 1))
      r2 <- putSecret conn WriteAllowed (SecretPath "test/api/versions") sd2
      r2 `shouldBe` Right (SecretVersion (startV + 2))
      r3 <- putSecret conn WriteAllowed (SecretPath "test/api/versions") sd3
      r3 `shouldBe` Right (SecretVersion (startV + 3))

    it "retrieves old version by number" $ \conn -> do
      startV <- getCurrentVersion conn "test/api/versions"
      result <- getSecret conn (SecretPath "test/api/versions") (Just (SecretVersion (startV - 2)))
      result `shouldBe` Right (toSecretData [("v", "one")])

  describe "deleteSecret + unDeleteSecretVersions" $ do
    it "soft-deletes a secret" $ \conn -> do
      let sd = toSecretData [("del", "me")]
      _ <- putSecret conn WriteAllowed (SecretPath "test/api/del") sd
      delResult <- deleteSecret conn (SecretPath "test/api/del")
      delResult `shouldSatisfy` isRight

    it "secret not readable after delete" $ \conn -> do
      getResult <- getSecret conn (SecretPath "test/api/del") Nothing
      getResult `shouldSatisfy` isLeft

    it "undeletes the secret" $ \conn -> do
      cv <- getCurrentVersion conn "test/api/del"
      undelResult <- unDeleteSecretVersions conn (SecretPath "test/api/del") (toSecretVersions [cv])
      undelResult `shouldSatisfy` isRight

    it "secret readable after undelete" $ \conn -> do
      cv <- getCurrentVersion conn "test/api/del"
      getResult <- getSecret conn (SecretPath "test/api/del") (Just (SecretVersion cv))
      getResult `shouldBe` Right (toSecretData [("del", "me")])

  describe "deleteSecretVersions" $ do
    it "deletes specific version only" $ \conn -> do
      startV <- getCurrentVersion conn "test/api/delver"
      let sd = toSecretData [("k", "v")]
      _ <- putSecret conn WriteAllowed (SecretPath "test/api/delver") sd
      _ <- putSecret conn WriteAllowed (SecretPath "test/api/delver") sd
      delResult <- deleteSecretVersions conn (SecretPath "test/api/delver") (toSecretVersions [startV + 1])
      delResult `shouldSatisfy` isRight

    it "remaining versions still accessible" $ \conn -> do
      cv <- getCurrentVersion conn "test/api/delver"
      result <- getSecret conn (SecretPath "test/api/delver") (Just (SecretVersion cv))
      result `shouldBe` Right (toSecretData [("k", "v")])

  describe "destroySecret" $ do
    it "permanently destroys a secret" $ \conn -> do
      let sd = toSecretData [("destroy", "now")]
      _ <- putSecret conn WriteAllowed (SecretPath "test/api/destroy") sd
      destroyResult <- destroySecret conn (SecretPath "test/api/destroy")
      destroyResult `shouldSatisfy` isRight

    it "metadata no longer available after destroy" $ \conn -> do
      mResult <- readSecretMetadata conn (SecretPath "test/api/destroy")
      mResult `shouldSatisfy` isLeft

  describe "destroySecretVersions" $ do
    it "destroys specific version only" $ \conn -> do
      startV <- getCurrentVersion conn "test/api/destver"
      let sd = toSecretData [("k", "v")]
      _ <- putSecret conn WriteAllowed (SecretPath "test/api/destver") sd
      _ <- putSecret conn WriteAllowed (SecretPath "test/api/destver") sd
      destroyResult <- destroySecretVersions conn (SecretPath "test/api/destver") (toSecretVersions [startV + 1])
      destroyResult `shouldSatisfy` isRight

    it "remaining versions still accessible" $ \conn -> do
      cv <- getCurrentVersion conn "test/api/destver"
      result <- getSecret conn (SecretPath "test/api/destver") (Just (SecretVersion cv))
      result `shouldBe` Right (toSecretData [("k", "v")])

  describe "secretsList" $ do
    it "lists secrets at a path" $ \conn -> do
      let sd = toSecretData [("listed", "yes")]
      _ <- putSecret conn WriteAllowed (SecretPath "test/api/list/secret1") sd
      _ <- putSecret conn WriteAllowed (SecretPath "test/api/list/secret2") sd
      listResult <- secretsList conn (SecretPath "test/api/list/")
      listResult `shouldSatisfy` isRight

  describe "kvEngineConfig" $ do
    it "configures KV engine settings" $ \conn -> do
      configResult <- kvEngineConfig conn 10 False
      configResult `shouldSatisfy` isRight

  describe "secretConfig" $ do
    it "configures per-path secret settings" $ \conn -> do
      configResult <- secretConfig conn (SecretPath "test/api/cfg") 5 True
      configResult `shouldSatisfy` isRight
