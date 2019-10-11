{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module URI.ByteString.Generators where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString     (ByteString)
import           Hedgehog
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
-------------------------------------------------------------------------------
import           Prelude
-------------------------------------------------------------------------------
import           URI.ByteString
-------------------------------------------------------------------------------



genUserInfo :: Gen UserInfo
genUserInfo = do
  username <- Gen.utf8 (Range.linear 0 100) Gen.ascii
  password <- Gen.utf8 (Range.linear 0 100) Gen.ascii
  pure $ UserInfo
    { uiUsername = username
    , uiPassword = password
    }


genAuthority :: Gen Authority
genAuthority = do
  userInfo <- Gen.maybe genUserInfo
  host <- genHost
  port <- Gen.maybe genPort
  pure $ Authority
    { authorityUserInfo = userInfo
    , authorityHost = host
    , authorityPort = port
    }


genHost :: Gen Host
genHost = Host <$> Gen.utf8 (Range.linear 0 100) Gen.ascii


genPort :: Gen Port
genPort = Port <$> genPositiveInt


genRelativeURIRef :: Gen (URIRef Relative)
genRelativeURIRef = do
  authority <- Gen.maybe genAuthority
  path <- Gen.utf8 (Range.linear 0 100) Gen.ascii
  query <- genQuery
  fragment <- Gen.maybe genAlphaNumBS
  pure $ RelativeRef
    { rrAuthority = authority
    , rrPath = path
    , rrQuery = query
    , rrFragment = fragment
    }


genAbsoluteURIRef :: Gen (URIRef Absolute)
genAbsoluteURIRef = do
  scheme <- genScheme
  authority <- Gen.maybe genAuthority
  path <- Gen.utf8 (Range.linear 0 100) Gen.ascii
  query <- genQuery
  fragment <- Gen.maybe genAlphaNumBS
  pure $ URI
    { uriScheme = scheme
    , uriAuthority = authority
    , uriPath = path
    , uriQuery = query
    , uriFragment = fragment
    }


genScheme :: Gen Scheme
genScheme = Scheme <$> genAlphaNumBS


genQuery :: Gen Query
genQuery = do
  pairs <- Gen.list (Range.linear 0 10) ((,) <$> genAlphaNumBS <*> genAlphaNumBS)
  pure $ Query
    { queryPairs = pairs
    }


genURIParserOptions :: Gen URIParserOptions
genURIParserOptions = do
  cointoss <- Gen.bool
  pure $ URIParserOptions
    { upoValidQueryChar = const cointoss
    }


genURINormalizationOptions :: Gen URINormalizationOptions
genURINormalizationOptions = do
  dScheme <- Gen.bool
  dHost <- Gen.bool
  dPort <- Gen.bool
  slashPath <- Gen.bool
  dSlashes <- Gen.bool
  dSort <- Gen.bool
  dSegments <- Gen.bool
  ports <- Gen.map (Range.linear 0 10) ((,) <$> genScheme <*> genPort)
  pure $ URINormalizationOptions
    { unoDowncaseScheme    = dScheme
    , unoDowncaseHost      = dHost
    , unoDropDefPort       = dPort
    , unoSlashEmptyPath    = slashPath
    , unoDropExtraSlashes  = dSlashes
    , unoSortParameters    = dSort
    , unoRemoveDotSegments = dSegments
    , unoDefaultPorts      = ports
    }


genSchemaError :: Gen SchemaError
genSchemaError = Gen.enumBounded


genURIParseError :: Gen URIParseError
genURIParseError = Gen.choice
  [ MalformedScheme <$> genSchemaError
  , pure MalformedUserInfo
  , pure MalformedQuery
  , pure MalformedFragment
  , pure MalformedHost
  , pure MalformedPort
  , pure MalformedPath
  , OtherError <$> genString
  ]


genString :: Gen String
genString = Gen.string (Range.linear 0 100) Gen.unicode


genAlphaNumBS :: Gen ByteString
genAlphaNumBS = Gen.utf8 (Range.linear 0 100) Gen.alphaNum


genBS :: Gen ByteString
genBS = Gen.utf8 (Range.linear 0 100) Gen.unicode


genPositiveInt :: Gen Int
genPositiveInt = Gen.int (Range.linear 0 maxBound)
