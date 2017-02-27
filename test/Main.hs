module Main (main) where

-------------------------------------------------------------------------------
import           Test.Tasty
-------------------------------------------------------------------------------
import qualified URI.ByteStringTests
import qualified URI.ByteStringQQTests
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite = testGroup "uri-bytestring"
  [
    URI.ByteStringTests.tests
  , URI.ByteStringQQTests.tests
  ]
