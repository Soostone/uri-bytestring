{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module URI.ByteStringQQTests (tests) where


-------------------------------------------------------------------------------
import           Test.Tasty
import           Test.Tasty.HUnit
-------------------------------------------------------------------------------
import           URI.ByteString
import           URI.ByteString.QQ
-------------------------------------------------------------------------------


quasiTest :: URI
quasiTest = [uri|https://stackage.org/foo?bar=baz#quux|]


quasiRelTest :: RelativeRef
quasiRelTest = [relativeRef|/foo?bar=baz#quux|]

tests :: TestTree
tests = testGroup "URI.ByteString.QQ"
  [ testCase "uri quasi quoter produces expected RelativeRef" $ do
      quasiTest @?= URI (Scheme "https") (Just (Authority Nothing (Host "stackage.org") Nothing)) "/foo" (Query [("bar", "baz")]) (Just "quux")
  , testCase "relativeRef quasi quoter produces expected RelativeRef" $ do
      quasiRelTest @?= RelativeRef Nothing "/foo" (Query [("bar", "baz")]) (Just "quux")
  ]
