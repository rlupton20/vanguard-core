import Test.Framework

import TypesTest (typesTest)
import ControlTest (controlTest)

import Prelude

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ typesTest
        , controlTest ]
