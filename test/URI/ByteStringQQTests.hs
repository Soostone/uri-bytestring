{-# LANGUAGE QuasiQuotes         #-}

module URI.ByteStringQQTests (quasiTest) where

import           URI.ByteString.QQ
import           URI.ByteString

quasiTest :: URI
quasiTest = [uri|https://stackage.org|]
