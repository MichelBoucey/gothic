{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Database.Vault.KVv2.Client.Types where

import           Control.Monad        (mzero)
import           Data.Aeson
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key       as K
import qualified Data.Aeson.KeyMap    as KM
#endif
import qualified Data.ByteString      as B
import           Data.HashMap.Strict
import           Data.Hashable
import           Data.Scientific
import qualified Data.Text            as T
import           Data.Text.Read       (decimal)
import           GHC.Generics
import           Network.HTTP.Client  (Manager)

import           Database.Vault.KVv2.Client.Internal

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
  show (VaultConnection a p _ _) =
    removeTrailingSlash a ++ "/v1/"
    ++ removeTrailingSlash (removeLeadingSlash p)

newtype SecretVersions =
  SecretVersions [SecretVersion]
  deriving (Show, Eq)

instance ToJSON SecretVersions where
  toJSON (SecretVersions svs) =
    object
      [ "versions" .= ((\(SecretVersion i) -> i) <$> svs) ]

newtype SecretVersion
  = SecretVersion Int
  deriving (Show, Eq, Generic, Hashable)

newtype SecretMetadata =
  SecretMetadata (HashMap SecretVersion Metadata)
  deriving (Show, Eq)

instance FromJSON SecretMetadata where
  parseJSON (Object o) =
#if MIN_VERSION_aeson(2,0,0)
    pure (SecretMetadata . fromList $ trans <$> toList (KM.toHashMap o))
#else
    pure (SecretMetadata . fromList $ trans <$> toList o)
#endif
    where
    trans p =
      case p of
        (k,j@(Object _)) -> do
#if MIN_VERSION_aeson(2,0,0)
          let Right (i,_) = decimal (K.toText k)
#else
          let Right (i,_) = decimal k
#endif
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
  = VaultKey    !String
  | VaultFolder !String
  deriving (Show) 

