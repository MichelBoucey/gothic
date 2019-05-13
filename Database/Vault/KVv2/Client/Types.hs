{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Vault.KVv2.Client.Types where

import           Data.Aeson                    as A
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
    { destroyed     :: Bool
    , deletion_time   :: T.Text
    , version       :: Int
    , created_time   :: T.Text
    } deriving (Show, Generic, FromJSON)


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
    , put_data :: SecretData }

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

data ResponseData =
  ResponseData
    { secret_data     :: SecretData
    , secret_metadata :: SecretMetadata
    } deriving (Show, Generic)

instance FromJSON ResponseData where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = toJSONName }

-- https://www.vaultproject.io/api/secret/kv/kv-v2.html
data VaultResponse =
  VaultResponse
    { lease_duration :: Int
    , wrap_info      :: Maybe (HashMap T.Text T.Text)
    , auth          :: Maybe VaultAuth
    , response_data  :: ResponseData
    , request_id     :: T.Text
    , warnings      :: Maybe (V.Vector T.Text)
    , lease_id     :: T.Text
    , renewable     :: Bool
    } deriving (Show, Generic)

instance FromJSON VaultResponse where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = toJSONName }

toJSONName :: String -> String
toJSONName "secret_data"     = "data"
toJSONName "secret_metadata" = "metadata"
toJSONName "response_data"   = "data"
toJSONName s                 = s

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
    , auth_metadata             :: Maybe (HashMap T.Text T.Text)
    } deriving (Show, Generic, ToJSON, FromJSON)


-- Secret | Folder
