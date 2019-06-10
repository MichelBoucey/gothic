{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Vault.KVv2.Client.Types where

import           Data.Aeson
import qualified Data.ByteString               as B
import           Data.HashMap.Strict
import           Data.Scientific
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           GHC.Generics
import           Network.HTTP.Client           (Manager)

type VaultToken = String

data VaultConnection =
  VaultConnection
    { vaultAddr         :: String
    , secretsEnginePath :: String
    , vaultToken        :: B.ByteString
    , manager           :: Manager
    }

data SecretVersion
  = LatestVersion
  | Version !Int
  deriving (Show)

newtype SecretVersions =
  SecretVersions (V.Vector Value)
  deriving (Show)

{-
Right (Object (fromList [("lease_duration",Number 0.0),("wrap_info",Null),("auth",Null),("data",Object (fromList [("oldest_version",Number 0.0),("versions",Object (fromList [("1",Object (fromList [("destroyed",Bool False),("deletion_time",String ""),("created_time",String "2019-06-06T12:50:00.414688723Z")])),("4",Object (fromList [("destroyed",Bool False),("deletion_time",String ""),("created_time",String "2019-06-06T13:03:25.138703084Z")])),("2",Object (fromList [("destroyed",Bool False),("deletion_time",String ""),("created_time",String "2019-06-06T12:50:40.757429676Z")])),("5",Object (fromList [("destroyed",Bool False),("deletion_time",String ""),("created_time",String "2019-06-06T13:04:02.508429438Z")])),("3",Object (fromList [("destroyed",Bool False),("deletion_time",String "2019-06-06T12:58:54.229644656Z"),("created_time",String "2019-06-06T12:52:18.725352687Z")]))])),("current_version",Number 5.0),("updated_time",String "2019-06-06T13:04:02.508429438Z"),("cas_required",Bool False),("created_time",String "2019-06-06T12:50:00.414688723Z"),("max_versions",Number 0.0)])),("request_id",String "8c9ad99c-b099-10f2-f34e-fa72d9440504"),("warnings",Null),("lease_id",String ""),("renewable",Bool False)]))
-}

instance ToJSON SecretVersions where
  toJSON (SecretVersions vs) =
    object
      [ "versions" .= Array vs ]

newtype SecretData =
  SecretData
    (HashMap T.Text T.Text)
    deriving (Show, Generic, ToJSON, FromJSON)

data SecretSettings =
  SecretSettings
    { max_versions :: Int
    , cas_required :: Bool
    } deriving (Show, Generic, ToJSON, FromJSON)
    
newtype SecretPath =
  SecretPath
    { path :: String }
    deriving (Show, Generic, ToJSON)

data CheckAndSet
  = WriteAllowed
  | CreateOnly
  | CurrentVersion Int
  deriving (Show, Generic, ToJSON)

newtype PutSecretOptions =
  PutSecretOptions
    { cas :: CheckAndSet }
    deriving (Show)

instance ToJSON PutSecretOptions where
  toJSON PutSecretOptions { cas = WriteAllowed }     = object []
  toJSON PutSecretOptions { cas = CreateOnly }       = object [ "cas" .= Number 0.0 ]
  toJSON PutSecretOptions { cas = CurrentVersion v } = object [ "cas" .= Number (read (show v) :: Scientific) ]

data PutSecretRequestBody =
  PutSecretRequestBody
    { options  :: PutSecretOptions
    , put_data :: SecretData
    }

instance ToJSON PutSecretRequestBody where
  toJSON (PutSecretRequestBody os sd) =
    object
      [ "options" .= os
      , "data"    .= sd
      ]

data VaultKey
  = VaultKey T.Text
  | VaultFolder T.Text
  deriving (Show) 

