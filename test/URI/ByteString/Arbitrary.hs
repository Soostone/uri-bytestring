{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module URI.ByteString.Arbitrary where


-------------------------------------------------------------------------------
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Data.Derive.Arbitrary     (makeArbitrary)
import           Data.DeriveTH             (derive)
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
-------------------------------------------------------------------------------
import           URI.ByteString
-------------------------------------------------------------------------------


instance Arbitrary UserInfo where
  arbitrary = UserInfo <$> arbitrary
                       <*> arbitrary


instance Arbitrary Authority where
  arbitrary = Authority <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary


instance Arbitrary Host where
  arbitrary = Host <$> arbitrary


instance Arbitrary Port where
  arbitrary = Port <$> arbitrary


instance Arbitrary URI where
  arbitrary = URI <$> arbitrary
                  <*> arbitrary
                  <*> arbitrary
                  <*> arbitrary
                  <*> arbitrary


instance Arbitrary RelativeRef where
  arbitrary = RelativeRef <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary


instance Arbitrary Scheme where
  arbitrary = Scheme <$> arbitrary


instance Arbitrary Query where
  arbitrary = Query <$> arbitrary


instance Arbitrary URIParserOptions where
  arbitrary = URIParserOptions <$> arbitrary


$(derive makeArbitrary ''SchemaError)
$(derive makeArbitrary ''URIParseError)
