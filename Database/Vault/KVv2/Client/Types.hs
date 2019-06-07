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

