{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Vault.KVv2.Client.Requests (

    configR,
    getSecretR,
    putSecretR,
    deleteSecretR,
    destroySecretR,
    secretVersionsR,
    readSecretMetadataR,
    secretsListR

  ) where

import qualified Data.Aeson                          as A
import           Network.HTTP.Client
import           Network.HTTP.Simple                 ( setRequestBody
                                                     , setRequestHeaders
                                                     , setRequestBodyJSON
                                                     )

import           Database.Vault.KVv2.Client.Internal
import           Database.Vault.KVv2.Client.Types

configR
  :: String                     -- ^ Endpoint
  ->VaultConnection
  -> Int                        -- ^ Max versions
  -> Bool                       -- ^ CAS required
  -> IO (Either String A.Value)
configR ep VaultConnection{..} mvs casr =
  parseRequest ep
  >>= runRequest manager
    . setRequestHeaders (vaultHeaders vaultToken)
    . setRequestBodyJSON
        SecretSettings
          { max_versions = mvs
          , cas_required = casr
          }

getSecretR
  :: VaultConnection
  -> SecretPath
  -> Maybe SecretVersion
  -> IO (Either String A.Value)
getSecretR VaultConnection{..} (SecretPath sp) msv =
  parseRequest (concat [vaultAddr, "/v1/", secretsEnginePath, "data/", sp, queryString msv])
  >>= runRequest manager . setRequestHeaders (vaultHeaders vaultToken)
  where
  queryString = maybe "" (\v -> "?version=" ++ show v) 
  
putSecretR
  :: VaultConnection
  -> CheckAndSet
  -> SecretPath
  -> SecretData
  -> IO (Either String A.Value)
putSecretR VaultConnection{..} cas (SecretPath sp) sd =
  parseRequest (concat ["POST ", vaultAddr, "/v1/", secretsEnginePath, "data/", sp ])
  >>= runRequest manager
    . setRequestHeaders (vaultHeaders vaultToken)
    . setRequestBodyJSON
        PutSecretRequestBody
          { options  = PutSecretOptions { cas = cas }
          , put_data = sd
          }

deleteSecretR
  :: VaultConnection
  -> SecretPath
  -> IO (Either String A.Value)
deleteSecretR VaultConnection{..} (SecretPath sp) =
  parseRequest (concat ["DELETE ", vaultAddr, "/v1/", secretsEnginePath, "data/", sp])
  >>= runRequest manager . setRequestHeaders (vaultHeaders vaultToken)

secretVersionsR
  :: String                     -- ^ Endpoint
  -> VaultConnection
  -> SecretVersions
  -> IO (Either String A.Value)
secretVersionsR ep VaultConnection{..} vs =
  parseRequest ep >>=
    runRequest manager
      . setRequestHeaders (vaultHeaders vaultToken)
      . setRequestBody (RequestBodyLBS $ A.encode vs)

destroySecretR
  :: VaultConnection
  -> SecretPath
  -> IO (Either String A.Value)
destroySecretR VaultConnection{..} (SecretPath sp) =
  parseRequest (concat ["DELETE ", vaultAddr, "/v1/", secretsEnginePath, "metadata/", sp])
  >>= runRequest manager . setRequestHeaders (vaultHeaders vaultToken)

secretsListR
  :: VaultConnection
  -> SecretPath
  -> IO (Either String A.Value)
secretsListR VaultConnection{..} (SecretPath sp) =
  if last sp /= '/'
    then return (Left "SecretPath must be a folder/")
    else parseRequest (concat ["LIST ", vaultAddr, "/v1/", secretsEnginePath, "metadata/", sp])
         >>= runRequest manager . setRequestHeaders (vaultHeaders vaultToken)

readSecretMetadataR
  :: VaultConnection
  -> SecretPath
  -> IO (Either String A.Value)
readSecretMetadataR VaultConnection{..} (SecretPath sp) =
  parseRequest (concat ["GET ", vaultAddr, "/v1/", secretsEnginePath, "metadata/", sp])
  >>= runRequest manager . setRequestHeaders (vaultHeaders vaultToken)

