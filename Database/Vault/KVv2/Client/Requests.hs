{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Vault.KVv2.Client.Requests (

    getSecretR,
    putSecretR,
    updateSecretMetadataR,
    deleteSecretR,
    deleteSecretVersionsR,
--    unDeleteSecretVersionsR,
    destroySecretR,
--    destroySecretVersionsR,
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
          { options = PutSecretOptions { cas = cas }
          , put_data = sd
          }

{- 
putSecretR -> (Version 1.0)
Object (fromList [("lease_duration",Number 0.0),("wrap_info",Null),("auth",Null),("data",Object (fromList [("destroyed",Bool False),("deletion_time",String ""),("version",Number 1.0),("created_time",String "2019-05-14T20:54:20.519889313Z")])),("request_id",String "52c3a8a0-1409-c41c-ba94-6eab8a4e4420"),("warnings",Null),("lease_id",String ""),("renewable",Bool False)])


putSecretR conn CreateOnly (SecretPath "Bob") (toSecretData [("Square","PANTS")])
Right (Object (fromList [("errors",Array [String "check-and-set parameter did not match the current version"])]))
-}

deleteSecretR
  :: VaultConnection
  -> SecretPath
  -> IO (Either String A.Value)
deleteSecretR VaultConnection{..} (SecretPath sp) =
  parseRequest (concat ["DELETE ", vaultAddr, "/v1/", secretsEnginePath, "data/", sp])
  >>= runRequest manager . setRequestHeaders (vaultHeaders vaultToken)

deleteSecretVersionsR
  :: VaultConnection
  -> SecretPath
  -> SecretVersions
  -> IO (Either String A.Value)
deleteSecretVersionsR VaultConnection{..} (SecretPath sp) vs =
  parseRequest (concat ["POST ", vaultAddr, "/v1/", secretsEnginePath, "delete/", sp])
  >>= runRequest manager
    . setRequestHeaders (vaultHeaders vaultToken)
    . setRequestBody (RequestBodyLBS $ A.encode vs)
{-
unDeleteSecretVersionsR
  :: VaultConnection
  -> SecretPath
  -> SecretVersions
  -> IO (Either String A.Value)
unDeleteSecretVersionsR VaultConnection{..} (SecretPath sp) vs =
  parseRequest (concat ["POST ", vaultAddr, "/v1/", secretsEnginePath, "undelete/", sp])
  >>= runRequest manager
    . setRequestHeaders (vaultHeaders vaultToken)
    . setRequestBody (RequestBodyLBS $ A.encode vs)
-}
destroySecretR
  :: VaultConnection
  -> SecretPath
  -> IO (Either String A.Value)
destroySecretR VaultConnection{..} (SecretPath sp) =
  parseRequest (concat ["DELETE ", vaultAddr, "/v1/", secretsEnginePath, "metadata/", sp])
  >>= runRequest manager . setRequestHeaders (vaultHeaders vaultToken)
{-
destroySecretVersionsR
  :: VaultConnection
  -> SecretPath
  -> SecretVersions
  -> IO (Either String A.Value)
destroySecretVersionsR VaultConnection{..} (SecretPath sp) vs =
  parseRequest (concat ["POST ", vaultAddr, "/v1/", secretsEnginePath, "destroy/", sp])
  >>= runRequest manager
    . setRequestHeaders (vaultHeaders vaultToken)
    . setRequestBody (RequestBodyLBS $ A.encode vs)
-}
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

updateSecretMetadataR
  :: VaultConnection
  -> SecretPath
  -> Int
  -> Bool
  -> IO (Either String A.Value)
updateSecretMetadataR VaultConnection{..} (SecretPath sp) mvs casr =
  parseRequest (concat ["POST ", vaultAddr, "/v1/", secretsEnginePath, "metadata/", sp])
  >>= runRequest manager
    . setRequestHeaders (vaultHeaders vaultToken)
    . setRequestBodyJSON
        SecretSettings
          { max_versions = mvs
          , cas_required = casr
          }

