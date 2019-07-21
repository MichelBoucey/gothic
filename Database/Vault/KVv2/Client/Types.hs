{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Vault.KVv2.Client.Types where

import           Control.Monad        (mzero)
import           Data.Aeson
-- import           Data.Binary -- TODO
import qualified Data.ByteString      as B
import           Data.HashMap.Strict
import           Data.Hashable
import           Data.Scientific
import qualified Data.Text            as T
import           Data.Text.Read       (decimal)
import           GHC.Generics
import           Network.HTTP.Client  (Manager)

type VaultToken = String

type Error = String

data VaultConnection =
  VaultConnection
    { vaultAddr    :: !String
    , kvEnginePath :: !String
    , vaultToken   :: !B.ByteString
    , manager      :: !Manager
    }

instance Show VaultConnection where
  show (VaultConnection va ep _ _) = va <> "/v1/" <> ep

data SecretVersions =
  SecretVersions [SecretVersion]
  deriving (Show, Eq)

instance ToJSON SecretVersions where
  toJSON (SecretVersions svs) =
    object
      [ "versions" .= ((\(SecretVersion i) -> i) <$> svs) ]

data SecretVersion
  = SecretVersion !Int
  deriving (Show, Eq, Generic, Hashable)

data SecretMetadata =
  SecretMetadata (HashMap SecretVersion Metadata)
  deriving (Show, Eq)

instance FromJSON SecretMetadata where
  parseJSON (Object o) =
    pure (SecretMetadata . fromList $ trans <$> toList o)
    where
    trans p =
      case p of
        (t,j@(Object _)) -> do
          let Right (i,_) = decimal t
          let Success sv  = fromJSON j
          (SecretVersion i,sv)
        _                -> undefined
  parseJSON _          = mzero

data Metadata =
  Metadata 
    { destroyed     :: !Bool
    , deletion_time :: !T.Text
    , created_time  :: !T.Text
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype SecretData =
  SecretData
    (HashMap T.Text T.Text)
    deriving (Show, Generic, ToJSON, FromJSON)

-- instance Binary SecretData -- TODO

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
  | CurrentVersion !Int
  deriving (Show, Generic, ToJSON)

newtype PutSecretOptions =
  PutSecretOptions
    { cas :: CheckAndSet }
    deriving (Show)

instance ToJSON PutSecretOptions where
  toJSON PutSecretOptions { cas = WriteAllowed } = object []
  toJSON PutSecretOptions { cas = CreateOnly }   = object [ "cas" .= Number 0.0 ]
  toJSON PutSecretOptions { cas = CurrentVersion v } =
    object [ "cas" .= Number (read (show v) :: Scientific) ]

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
  = VaultKey !String
  | VaultFolder !String
  deriving (Show) 

