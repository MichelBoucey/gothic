{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Vault.KVv2.Client (

    -- * Connect and configure Vault KV v2 Engine
    vaultConnect,
    kvEngineConfig,
    secretConfig,
  
    -- * Basic operations

    getSecret,
    putSecret,
  
    -- * Soft secret deletion
    deleteSecret,
    deleteSecretVersions,
    unDeleteSecretVersions,
  
    -- * Permanent secret deletion
    destroySecret,
    destroySecretVersions,
  
    -- * Get informations

    secretCurrentVersion,
    readSecretMetadata,
    secretsList,

    -- * Utils
    toSecretData,
    toSecretVersions,

  ) where

import qualified Data.Aeson                          as A
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Char8               as C
import           Data.HashMap.Strict
import qualified Data.Maybe                          as M
import           Data.Text                           hiding (concat)
import           Network.Connection 
import           Network.HTTP.Client.TLS
import           System.Environment                  (lookupEnv)
import           System.Posix.Files                  (fileExist)

import           Database.Vault.KVv2.Client.Types
import           Database.Vault.KVv2.Client.Lens
import           Database.Vault.KVv2.Client.Requests

-- | Get a 'VaultConnection' or an error message.
vaultConnect
  :: Maybe String                       -- ^ Use Just this string as Vault address or get it from VAULT_ADDR
  -> String                             -- ^ Secrets engine path
  -> Maybe VaultToken                   -- ^ Use Just this Vault token or get it from $HOME/.vaut-token
  -> Bool                               -- ^ Disable certificate validation
  -> IO (Either String VaultConnection)
vaultConnect mva sep mvt dcv = do
  nm <- newTlsManagerWith $
          mkManagerSettings
            TLSSettingsSimple
              { settingDisableCertificateValidation = dcv
              , settingDisableSession               = False
              , settingUseServerName                = True
              }
            Nothing
  va <- case mva of
          Just va -> return (Just va)
          Nothing -> lookupEnv "VAULT_ADDR"
  evt <- case mvt of
           Just t  -> return (Right $ C.pack t)
           Nothing -> do
             hm <- lookupEnv "HOME"
             if M.isJust hm
               then do
                 let pf = M.fromJust hm ++ "/.vault-token"
                 if M.isJust va
                   then do
                     fe <- fileExist pf
                     if fe 
                       then Right <$> B.readFile pf
                       else return (Left $ "No Vault token file found at " ++ pf)
                   else return (Left "Variable environnment VAULT_ADDR not set")
               else return (Left "Variable environnment VAULT_ADDR not set")
  pure $
    (\vt ->
      VaultConnection
        { vaultAddr         = M.fromJust va
        , vaultToken        = vt
        , secretsEnginePath = sep
        , manager           = nm
        }
    ) <$> evt

kvEngineConfig
  :: VaultConnection
  -> Int                        -- ^ Max versions
  -> Bool                       -- ^ CAS required
  -> IO (Either String A.Value)
kvEngineConfig vc@VaultConnection{..} =
  configR (concat ["POST ", vaultAddr, "/v1/", secretsEnginePath, "config"]) vc


secretConfig
  :: VaultConnection
  -> SecretPath
  -> Int                        -- ^ Max versions
  -> Bool                       -- ^ CAS required
  -> IO (Either String A.Value)
secretConfig vc@VaultConnection{..} (SecretPath sp) =
  configR (concat ["POST ", vaultAddr, "/v1/", secretsEnginePath, "metadata/", sp ]) vc

-- | Get a secret from Vault.
getSecret
  :: VaultConnection
  -> SecretPath
  -> Maybe SecretVersion
  -> IO (Either String SecretData)
getSecret vc sp msv =
  secret <$> getSecretR vc sp msv

-- | Put a secret in Vault.
putSecret
  :: VaultConnection
  -> CheckAndSet
  -> SecretPath
  -> SecretData
  -> IO (Either String SecretVersion)
putSecret vc cas sp sd =
  version <$> putSecretR vc cas sp sd

deleteSecret
  :: VaultConnection
  -> SecretPath
  -> IO (Maybe Error)
deleteSecret vc sp =
  maybeError <$> deleteSecretR vc sp

deleteSecretVersions
  :: VaultConnection
  -> SecretPath
  -> SecretVersions
  -> IO (Maybe Error)
deleteSecretVersions vc@VaultConnection{..} (SecretPath sp) svs =
  maybeError <$> secretVersionsR (concat ["POST ", vaultAddr, "/v1/", secretsEnginePath, "delete/", sp]) vc svs

unDeleteSecretVersions
  :: VaultConnection
  -> SecretPath
  -> SecretVersions
  -> IO (Maybe Error)
unDeleteSecretVersions vc@VaultConnection{..} (SecretPath sp) svs =
  maybeError <$> secretVersionsR (concat ["POST ", vaultAddr, "/v1/", secretsEnginePath, "undelete/", sp]) vc svs

destroySecret
  :: VaultConnection
  -> SecretPath
  -> IO (Maybe Error)
destroySecret vc sp =
  maybeError <$> destroySecretR vc sp

destroySecretVersions
  :: VaultConnection
  -> SecretPath
  -> SecretVersions
  -> IO (Either String A.Value)
destroySecretVersions vc@VaultConnection{..} (SecretPath sp) =
  secretVersionsR (concat ["POST ", vaultAddr, "/v1/", secretsEnginePath, "destroy/", sp]) vc

secretsList
  :: VaultConnection
  -> SecretPath
  -> IO (Either String [VaultKey])
secretsList vc sp =
  list <$> secretsListR vc sp

readSecretMetadata
  :: VaultConnection
  -> SecretPath
  -> IO (Either String SecretMetadata)
readSecretMetadata vc sp =
  metadata <$> readSecretMetadataR vc sp

secretCurrentVersion
  :: VaultConnection
  -> SecretPath
  -> IO (Either String SecretVersion)
secretCurrentVersion vc sp =
  current <$> readSecretMetadataR vc sp

-- Utils

toSecretData
  :: [(Text,Text)]
  -> SecretData
toSecretData = SecretData . fromList

toSecretVersions
  :: [Int]
  -> SecretVersions
toSecretVersions is = SecretVersions (SecretVersion <$> is)

