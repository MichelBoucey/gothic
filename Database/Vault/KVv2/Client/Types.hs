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

instance ToJSON SecretVersions where
  toJSON (SecretVersions vs) =
    object
      [ "versions" .= Array vs ]

newtype SecretData =
  SecretData
    (HashMap T.Text T.Text)
    deriving (Show, Generic, ToJSON, FromJSON)

data SecretMetadata =
  SecretMetadata
    { destroyed     :: Bool
    , deletion_time :: T.Text
    , version       :: Int
    , created_time  :: T.Text
    } deriving (Show, Generic, FromJSON)


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
{-
data ResponseData =
  ResponseData
    { secret_data     :: SecretData
    , secret_metadata :: SecretMetadata
    } deriving (Show, Generic)

instance FromJSON ResponseData where
  parseJSON =
    genericParseJSON
      defaultOptions { fieldLabelModifier = toJSONName }
-}

data VaultItem
  = Key T.Text
  | Folder T.Text
  deriving (Show) 

{-

instance FromJSON VaultItem where
  parseJSON (Object o) 
  parseJSON (A.Object v) = do
    ar <- v A..: "keys"
    return listKeys V.toList ar
  parseJSON _ = mzero

Right (Object (fromList [("lease_duration",Number 0.0),("wrap_info",Null),("auth",Null),("data",Object (fromList [("keys",Array [String "dfdfg",String "sdf",String "sub/"])])),("request_id",String "e14a36ca-2893-2526-f976-0a7b9d4735c2"),("warnings",Null),("lease_id",String ""),("renewable",Bool False)]))

-}
{-
data VaultResponse =
  VaultResponse
    { lease_duration :: Int
    , wrap_info      :: Maybe (HashMap T.Text T.Text)
    , auth           :: Maybe VaultAuth
    , response_data  :: ResponseData
    , request_id     :: T.Text
    , warnings       :: Maybe (V.Vector T.Text)
    , lease_id       :: T.Text
    , renewable      :: Bool
    } deriving (Show, Generic)

instance FromJSON VaultResponse where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = toJSONName }

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
-}
listKeys :: [Value] -> ([VaultItem],[VaultItem])
listKeys =
  foldl lks ([],[])
  where
    lks (ks,fs) (String t) =
      if T.last t == '/'
        then (ks,fs++[Folder t])
        else (ks++[Key t],fs)
    lks p       _          = p

