{-# LANGUAGE OverloadedStrings #-}

module Database.Vault.KV.V2.Types where

import qualified Data.Aeson                    as A
-- import qualified Data.Aeson.Lens               as L
import qualified Data.ByteString               as B
import           Data.HashMap.Strict           as HM
import qualified Data.Vector as V
import           Data.HashSet
import           Network.HTTP.Client           (Manager)
import qualified Data.Text                     as T
import Control.Monad

data VaultAPIVersion = V1

instance Show VaultAPIVersion where
  show V1 = "v1"

data VaultConfig =
  VaultConfig
    { vaultToken :: B.ByteString
    , vaultAddr :: String
    , secretsEnginePath :: String
    } deriving (Show)

data VaultConnection =
  VaultConnection
    { config  :: VaultConfig
    , manager :: Manager
    }

newtype Versions = Versions (HashSet Int) deriving (Show)

data SecretVersion = LatestVersion
                   | Version !Int
                   deriving (Show)

newtype SecretData = SecretData (HashMap T.Text T.Text) 

-- TODO instance A.ToJSON SecretData where

data SecretMetadata =
  SecretMetadata
    { createdTime   :: T.Text
    , version       :: Int
    , deletionTime  :: T.Text
    , destroyed     :: Bool
    } deriving (Show)

instance A.ToJSON SecretMetadata where
  toJSON (SecretMetadata c v l s) =
    A.object [ "created_time"  A..= c
             , "version"       A..= v
             , "deletion_time" A..= l
             , "destroyed"     A..= s
             ]

instance A.FromJSON SecretMetadata where
  parseJSON (A.Object v) =
    SecretMetadata <$>
      v A..: "created_time"  <*>
      v A..: "version"       <*>
      v A..: "deletion_time" <*>
      v A..: "destroyed"
  parseJSON _            = mzero

newtype SecretPath = SecretPath { unSecretPath :: String } deriving (Show)

{-
 (Response {responseStatus = Status {statusCode = 200, statusMessage = "OK"}, responseVersion = HTTP/1.1, responseHeaders = [("Cache-Control","no-store"),("Content-Type","application/json"),("Date","Sat, 01 Dec 2018 10:10:25 GMT"),("Content-Length","294")], responseBody = Object (fromList [("lease_duration",Number 0.0),("wrap_info",Null),("auth",Null),("data",Object (fromList [("data",Object (fromList [("michel",String "True")])),("metadata",Object (fromList [("destroyed",Bool False),("deletion_time",String ""),("version",Number 1.0),("created_time",String "2018-12-01T07:12:25.806880744Z")]))])),("request_id",String "3667a85c-ff92-48fb-c265-1b38a3dc2b32"),("warnings",Null),("lease_id",String ""),("renewable",Bool False)])

https://github.com/hashicorp/vault/blob/5269abb64c878aabbf91d0e54befb314630fae12/api/secret.go

"auth": {
    "client_token": "af5f7682-aa55-fa37-5039-ee116df56600",
    "accessor": "19b5407e-b304-7cde-e946-54942325d3c1",
    "policies": [
      "apps",
      "default"
    ],

-}

data VaultKVResponse =
  VaultSecret 
    { leaseDuration :: Int
    , wrapInfo      :: Maybe (HashMap T.Text T.Text)
    , auth          :: Maybe VaultAuth
    , secretData    :: SecretData
    , requestId     :: T.Text
    , warnings      :: Maybe (V.Vector T.Text)
    , leaseId       :: T.Text
    , renewable     :: Bool
    }

-- github.com/hashicorp/vault/logical/auth.go
data VaultAuth =
  VaultAuth
    { clientToken :: T.Text
    , accessor :: T.Text
    , period :: Maybe Int
    , explicitMaxTTL :: Maybe Int
    , numUses :: Maybe Int
    , entityID :: Maybe T.Text
    , policies :: Maybe (V.Vector T.Text)
    , tokenPolicies :: Maybe (V.Vector T.Text)
    , identityPolicies :: Maybe (V.Vector T.Text)
    , externalNamespacePolicies :: Maybe (HashMap T.Text T.Text)
    , metadata :: Maybe (HashMap T.Text T.Text)
    }
