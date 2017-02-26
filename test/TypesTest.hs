module TypesTest
( typesTest ) where

import Test.Framework as TF
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit as HU
import Test.HUnit ((@=?), (~:))
import Test.QuickCheck
import Data.Word

import Network.Vanguard.Core

import Prelude

type Label = String
type Input = String
type Expected a = a

-- |typesTest collects together all the tests for the file
-- Types.hs in the test-framework format.
typesTest :: TF.Test
typesTest = testGroup "Test.hs tests:" $ [addrTests]

-- addr: tests for the parsing function addr

-- |addrTests collects all the tests pertaining to the
-- function addr.
addrTests :: TF.Test
addrTests = TF.testGroup "addr tests:" $ [ TF.testGroup "addr unit tests:" $ hUnitTestToTests addrUnitTests
                                         , TF.testGroup "addr QuickCheck:" $ [quickCheckAddr, quickCheckAddrOnBad] ]


-- |addrConversionTest creates a test for each input and expected outcome
addrConversionTest :: (String, String, Expected (Maybe Addr)) -> HU.Test
addrConversionTest (label, input, expected) = label ~: expected @=? (addr input :: Maybe Addr)

-- |addrConversionRuns describes various inputs and expected outcomes,
-- along with some strings describing the tests that are built from these.
addrConversionRuns :: [(String, String, Maybe Addr)]
addrConversionRuns = [ ("addr: doesn't convert \"String\"", 	   "String", Nothing)
		     , ("addr: addr and addrW8 agree", 		   "1.2.34.123", Just $ addrW8 1 2 34 123)
		     , ("addr: fail on \"1.43.t.90\"",		   "1.43.t.90", Nothing) 
		     , ("addr: fails on \"...\"",		   "...", Nothing)
                     , ("addr: fails on \"1.2.3.4.5\"",            "1.2.3.4.5", Nothing) ]


-- |addrUnitTests is the collection of unit tests for addr (built using
-- addrConversionTest mapped over addrConversionRuns).
addrUnitTests :: HU.Test
addrUnitTests = TestList $ map addrConversionTest addrConversionRuns

-- |quickCheckAddr is a QuickCheck that addr reads randomly generated
-- IP addresses.
quickCheckAddr :: TF.Test
quickCheckAddr = testProperty "addr: reads generated IPs correctly" $ addrReads
  where
    addrReads :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
    addrReads a b c d = (Just $ addrW8 a b c d) == (addr $ show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d)

quickCheckAddrOnBad :: TF.Test
quickCheckAddrOnBad = testProperty "addr: doesn't parse strings that aren't IP addresses" $ addrRandomParse
  where
    addrRandomParse :: String -> Bool
    addrRandomParse str = ( (addr str)==Nothing ) || ( (Just str)==(fmap renderAddr $ addr str) )
