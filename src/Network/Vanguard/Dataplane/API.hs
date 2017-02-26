{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Network.Vanguard.Dataplane.API where

import qualified Data.Aeson as A
import Data.Aeson.Types (Parser)
import Data.Aeson ((.:), (.=))
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import GHC.Generics (Generic)
import Network.Socket (Socket, PortNumber)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar ( TVar, newTVarIO
                                   , modifyTVar', readTVar, writeTVar)
import Control.Exception (bracketOnError)

import Network.Vanguard.Core
import Prelude

-- |Provide some FromJSON and ToJSON instances for Connection types
-- First we need instances for PortNumbers
instance A.FromJSON PortNumber where
  parseJSON jn@(A.Number n) = fromIntegral <$> (A.parseJSON jn :: Parser Int)
  parseJSON _ = mempty

instance A.ToJSON PortNumber where
  toJSON n = A.toJSON (fromIntegral n :: Int)

instance A.FromJSON Connection
instance A.ToJSON Connection

-- |Request messages we may obtain over the control socket
data Request = Develop Connection
             | Connect Int Connection
             | New
             | BadRequest deriving (Eq, Show)

-- |Responses we may send over the control socket
data Response = ListeningOn Int Connection
              | ConnectingWith Connection
              | OK deriving (Show)

-- |JSON parser for Requests
instance A.FromJSON Request where
  parseJSON (A.Object o) = case HM.lookup "request" o of
    Just (A.String "develop") -> case HM.lookup "endpoint" o of
      Just (A.Object _) -> Develop <$> o .: "endpoint"
      _ -> pure BadRequest
    Just (A.String "connect") -> case HM.lookup "uid" o of
      Just (A.Number _) -> case HM.lookup "endpoint" o of
        Just (A.Object _) -> Connect <$> o .: "uid" <*> o .: "endpoint"
        _ -> pure BadRequest
      _ -> pure BadRequest
    Just (A.String "new") -> pure New
    _ -> pure BadRequest

-- |JSON writer for Responses
instance A.ToJSON Response where
  toJSON (ListeningOn uid connection) = A.object $
    [ "listening" .= A.Number (fromIntegral uid)
    , "endpoint" .= A.toJSON connection ]
  toJSON (ConnectingWith connection) = A.object $
    [ "connecting" .= A.toJSON connection ]
  toJSON OK = A.String "OK"
