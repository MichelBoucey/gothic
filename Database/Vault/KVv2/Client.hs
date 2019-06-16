{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Vault.KVv2.Client (

    -- * Connection and configure Vault KV v2 Engine
    vaultConnect,
    kvEngineConfig,
  
    -- * Basic operations

    getSecret,
    putSecret,
--    updateSecretMetadata,
  
    -- * Soft secret deletion
--    deleteSecret,
    deleteSecretVersions,
--    unDeleteSecretVersions,
  
    -- * Permanent secret deletion
--    destroySecret,
--    destroySecretVersions,
  
    -- * Get information

    readSecretMetadata,
    secretsList,

    -- * Utils
    -- toSecretData,
    toSecretVersions,

  ) where

import qualified Data.Aeson                          as A
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Char8               as C
import qualified Data.Maybe                          as M
import           Network.Connection 
import           Network.HTTP.Client
import           Network.HTTP.Simple                 ( setRequestHeaders
                                                     , setRequestBodyJSON
                                                     )
import           Network.HTTP.Client.TLS
import           System.Environment                  (lookupEnv)
import           System.Posix.Files                  (fileExist)

import           Database.Vault.KVv2.Client.Internal
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
        { vaultAddr = M.fromJust va
        , vaultToken = vt
        , secretsEnginePath = sep
        , manager = nm
        }
    ) <$> evt

kvEngineConfig
  :: VaultConnection
  -> Int                        -- ^ Max versions
  -> Bool                       -- ^ CAS required
  -> IO (Either String A.Value)
kvEngineConfig VaultConnection{..} mvs casr =
  parseRequest (concat ["POST ", vaultAddr, "/v1/", secretsEnginePath, "config"])
  >>= runRequest manager
    . setRequestHeaders (vaultHeaders vaultToken)
    . setRequestBodyJSON
        SecretSettings
          {  max_versions = mvs
          , cas_required = casr
          }

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

{-
secretCurrentVersion

deleteSecret
  :: VaultConnection
  -> SecretPath
  -> IO (Either String A.Value)
deleteSecret VaultConnection{..} (SecretPath sp) =
-}
deleteSecretVersions
  :: VaultConnection
  -> SecretPath
  -> SecretVersions
  -> IO (Either String A.Value)
deleteSecretVersions vc sp vs =
  deleteSecretVersionsR vc sp vs
{-
unDeleteSecretVersions
  :: VaultConnection
  -> SecretPath
  -> SecretVersions
  -> IO (Either String A.Value)
unDeleteSecretVersions VaultConnection{..} (SecretPath sp) vs =

destroySecret
  :: VaultConnection
  -> SecretPath
  -> IO (Either String A.Value)
destroySecret VaultConnection{..} (SecretPath sp) =

destroySecretVersions
  :: VaultConnection
  -> SecretPath
  -> SecretVersions
  -> IO (Either String A.Value)
destroySecretVersions VaultConnection{..} (SecretPath sp) vs =
-}

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
{-
updateSecretMetadata
  :: VaultConnection
  -> SecretPath
  -> Int                        -- ^ Max versions
  -> Bool                       -- ^ CAS required
  -> IO (Either String A.Value)
updateSecretMetadata VaultConnection{..} (SecretPath sp) mvs casr =
-}

-- Utils
{-
toSecretData
  :: [(T.Text,T.Text)]
  -> SecretData
toSecretData l =
  SecretData (HM.fromList l)

toSecretVersions :: [Int] -> SecretVersions
toSecretVersions is = SecretVersions $ V.fromList $ A.toJSON <$> is
-}

toSecretVersions :: [Int] -> SecretVersions
toSecretVersions is = SecretVersions $ (\i -> SecretVersion i) <$> is

