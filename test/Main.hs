module Main (main) where

-------------------------------------------------------------------------------
import Test.Tasty
-------------------------------------------------------------------------------
import URI.ByteStringTests
import URI.ByteString.LensTests
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite = testGroup "uri-bytestring"
  [
    URI.ByteStringTests.tests
  , URI.ByteString.LensTests.tests
  ]
