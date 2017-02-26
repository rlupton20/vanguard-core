{-# LANGUAGE OverloadedStrings #-}
module ControlTest ( controlTest ) where

import Test.Framework as TF
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit as HU
import Test.HUnit ((~:),(@=?))

import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString, append)

import Network.Vanguard.Dataplane.API
import Network.Vanguard.Core

import Prelude

controlTest :: TF.Test
controlTest = testGroup "Control.hs tests" $ hUnitTestToTests $
               HU.TestList [ testRejectsBadRequest
                           , testCanParseDevelopConnection
                           , testRejectsBadDataOnDevelopConnection
                           , testCanParseConnectMessage
                           , testCanParseNewConnection ]

testRejectsBadRequest :: HU.Test
testRejectsBadRequest = "Request: check bad request is recognized" ~: test
  where
    test = let json = "{\"this is bad json\" : [1,2,3,4] }"
               expected = BadRequest in
             Just expected @=? decode json


testCanParseDevelopConnection :: HU.Test
testCanParseDevelopConnection = "Develop: Can parse develop connection command" ~: test
  where
    test = let json = "{ \"request\" : \"develop\", " `append`
                        "\"endpoint\" : {" `append`
                          "\"virtual_ip\" : [1,2,211,56]," `append`
                          "\"external_ip\": [44,33,22,11]," `append`
                          "\"port\": 678 }}"
               (Just vip) = addr "1.2.211.56"
               (Just ip) = addr "44.33.22.11"
               port = 678
               expected = Develop $ Connection vip ip port in
             Just expected @=? decode json


testRejectsBadDataOnDevelopConnection :: HU.Test
testRejectsBadDataOnDevelopConnection = "Develop: Reject bad data on new connection" ~: test
  where
    test = let json = "{ \"request\" : \"develop\", " `append`
                        "\"endpoint\" : 5 }"
               expected = BadRequest in
             Just expected @=? decode json

testCanParseConnectMessage :: HU.Test
testCanParseConnectMessage = "Connect: Can parse connect message" ~: test
  where
    test = let json = "{\"request\" : \"connect\", \"uid\" : 234," `append`
                       "\"endpoint\" : {" `append`
                          "\"virtual_ip\" : [11,22,33,44]," `append`
                          "\"external_ip\": [255,127,63,31]," `append`
                          "\"port\": 58000 }}"
               (Just vip) = addr "11.22.33.44"
               (Just ip) = addr "255.127.63.31"
               port = 58000
               expected = Connect 234 $ Connection vip ip port in
             Just expected @=? decode json

testCanParseNewConnection :: HU.Test
testCanParseNewConnection = "New: test can parse new connection" ~: test
  where
    test = let json = "{ \"request\" : \"new\" }"
               expected = New in
             Just expected @=? decode json
