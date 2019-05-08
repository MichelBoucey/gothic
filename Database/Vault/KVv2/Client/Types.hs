{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Vault.KVv2.Client.Types where

import           Data.Aeson
import qualified Data.ByteString               as B
import           Data.HashMap.Strict
import           Data.HashSet
import           Data.Scientific
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           GHC.Generics
import           Network.HTTP.Client           (Manager)

data VaultConfig =
  VaultConfig
    { vaultAddr         :: String
    , secretsEnginePath :: String
    , vaultToken        :: B.ByteString
    } deriving (Show)

data VaultConnection =
  VaultConnection
    { config  :: VaultConfig
    , manager :: Manager }

data SecretVersion = LatestVersion
                   | Version !Int
                   deriving (Show)

newtype SecretVersions =
  SecretVersions (HashSet Int)
  deriving (Show, Generic, ToJSON, FromJSON)

newtype SecretData =
  SecretData
    (HashMap T.Text T.Text)
    deriving (Show, Generic, ToJSON, FromJSON)

data SecretMetadata =
  SecretMetadata
    { createdTime   :: T.Text
    , version       :: Int
    , deletionTime  :: T.Text
    , destroyed     :: Bool
    } deriving (Show, Generic, ToJSON, FromJSON)

newtype SecretPath =
  SecretPath
    { path :: String }
    deriving (Show, Generic, ToJSON)

data CheckAndSet = CreateOnly        -- cas == 0
                 | CreateUpdate      -- cas not set
                 | UpdateVersion Int -- cas > 0
                 deriving (Show, Generic, ToJSON)

data PutSecretOptions =
  PutSecretOptions
    { cas :: CheckAndSet }
    deriving (Show)

instance ToJSON PutSecretOptions where
  toJSON PutSecretOptions { cas = CreateOnly }      = object [ "cas" .= Number 0 ]
  toJSON PutSecretOptions { cas = CreateUpdate }    = object []
  toJSON PutSecretOptions { cas = UpdateVersion v } = object [ "cas" .= Number (read (show v) :: Scientific) ]

data PutSecretRequestBody =
  PutSecretRequestBody
    { options     :: PutSecretOptions
    , secret_data :: SecretData }

instance ToJSON PutSecretRequestBody where
  toJSON (PutSecretRequestBody os sd) =
    object
      [ "options" .= os,
        "data"    .= sd ]

{-
https://github.com/hashicorp/vault/blob/5269abb64c878aabbf91d0e54befb314630fae12/api/secret.go

"auth": {
    "client_token": "af5f7682-aa55-fa37-5039-ee116df56600",
    "accessor": "19b5407e-b304-7cde-e946-54942325d3c1",
    "policies": [
      "apps",
      "default"
    ],
-}

-- https://www.vaultproject.io/api/secret/kv/kv-v2.html
data VaultResponse =
  VaultResponse
    { leaseDuration :: Int
    , wrapInfo      :: Maybe (HashMap T.Text T.Text)
    , auth          :: Maybe VaultAuth
    , secretData    :: SecretData
    , requestId     :: T.Text
    , warnings      :: Maybe (V.Vector T.Text)
    , leaseId       :: T.Text
    , renewable     :: Bool
    } deriving (Show, Generic, ToJSON, FromJSON)
  -- | VaultErrors ?

-- github.com/hashicorp/vault/logical/auth.go
data VaultAuth =
  VaultAuth
    { clientToken               :: T.Text
    , accessor                  :: T.Text
    , period                    :: Maybe Int
    , explicitMaxTTL            :: Maybe Int
    , numUses                   :: Maybe Int
    , entityID                  :: Maybe T.Text
    , policies                  :: Maybe (V.Vector T.Text)
    , tokenPolicies             :: Maybe (V.Vector T.Text)
    , identityPolicies          :: Maybe (V.Vector T.Text)
    , externalNamespacePolicies :: Maybe (HashMap T.Text T.Text)
    , metadata                  :: Maybe (HashMap T.Text T.Text)
    } deriving (Show, Generic, ToJSON, FromJSON)


-- Secret | Folder
