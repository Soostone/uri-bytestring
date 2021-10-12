module Main (main) where

-------------------------------------------------------------------------------
import Test.Tasty
-------------------------------------------------------------------------------

import qualified URI.ByteStringQQTests
import qualified URI.ByteStringTests

-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite =
  testGroup
    "uri-bytestring"
    [ URI.ByteStringTests.tests,
      URI.ByteStringQQTests.tests
    ]
