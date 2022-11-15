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
import           Network.HTTP.Simple                 (setRequestBody,
                                                      setRequestBodyJSON,
                                                      setRequestHeaders)

import           Database.Vault.KVv2.Client.Internal
import           Database.Vault.KVv2.Client.Types

configR
  :: [String]                   -- ^ Endpoint
  ->VaultConnection
  -> Int                        -- ^ Max versions
  -> Bool                       -- ^ CAS required
  -> IO (Either String A.Value)
configR ss VaultConnection{..} mvs casr =
  parseRequest (concat ss)
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
getSecretR vc@VaultConnection{..} SecretPath{..} msv =
  parseRequest
    (concat [show vc, "/data/", path, queryString msv])
  >>= runRequest manager . setRequestHeaders (vaultHeaders vaultToken)
  where
  queryString = maybe "" (\(SecretVersion v) -> "?version=" ++ show v)

putSecretR
  :: VaultConnection
  -> CheckAndSet
  -> SecretPath
  -> SecretData
  -> IO (Either String A.Value)
putSecretR vc@VaultConnection{..} cas SecretPath{..} sd =
  parseRequest
    (concat ["POST ", show vc, "/data/", path])
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
deleteSecretR vc@VaultConnection{..} SecretPath{..} =
  parseRequest
    (concat ["DELETE ", show vc, "/data/", path])
  >>= runRequest manager . setRequestHeaders (vaultHeaders vaultToken)

secretVersionsR
  :: [String]                    -- ^ Endpoint
  -> VaultConnection
  -> SecretVersions
  -> IO (Either String A.Value)
secretVersionsR ss VaultConnection{..} vs =
  parseRequest (concat ss) >>=
    runRequest manager
      . setRequestHeaders (vaultHeaders vaultToken)
      . setRequestBody (RequestBodyLBS $ A.encode vs)

destroySecretR
  :: VaultConnection
  -> SecretPath
  -> IO (Either String A.Value)
destroySecretR vc@VaultConnection{..} SecretPath{..} =
  parseRequest
    (concat ["DELETE ", show vc, "/metadata/", path])
  >>= runRequest manager . setRequestHeaders (vaultHeaders vaultToken)

secretsListR
  :: VaultConnection
  -> SecretPath
  -> IO (Either String A.Value)
secretsListR vc@VaultConnection{..} SecretPath{..} =
  if hasTrailingSlash path
    then
      parseRequest
        (concat ["LIST ", show vc, "/metadata/", path])
      >>= runRequest manager . setRequestHeaders (vaultHeaders vaultToken)
    else pure (Left "SecretPath must be a folder/")

readSecretMetadataR
  :: VaultConnection
  -> SecretPath
  -> IO (Either String A.Value)
readSecretMetadataR vc@VaultConnection{..} SecretPath{..} =
  parseRequest
    (concat ["GET ", show vc, "/metadata/", path])
  >>= runRequest manager . setRequestHeaders (vaultHeaders vaultToken)

