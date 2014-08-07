module Main (main) where

import Test.Tasty

import URI.ByteStringTests

main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite = testGroup "uri-bytestring" [URI.ByteStringTests.tests]
