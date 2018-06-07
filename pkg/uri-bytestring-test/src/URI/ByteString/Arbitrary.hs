{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module URI.ByteString.Arbitrary where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Proxy                (Proxy (..))
import qualified Generics.SOP              as SOP
import qualified Generics.SOP.Constraint   as SOP
import qualified Generics.SOP.GGP          as SOP
import           GHC.Generics              (Generic)
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
-------------------------------------------------------------------------------
import           Prelude
-------------------------------------------------------------------------------
import           URI.ByteString
-------------------------------------------------------------------------------


-- this workaround can go away when
-- <https://github.com/nick8325/quickcheck/pull/40> is merged.
sopArbitrary
  :: ( SOP.SListI (SOP.GCode b)
     , Generic b
     , SOP.GTo b
     , SOP.AllF SOP.SListI (SOP.GCode b)
     , SOP.AllF (SOP.All Arbitrary) (SOP.GCode b)
     )
  => Gen b
sopArbitrary = fmap SOP.gto sopArbitrary'


sopArbitrary'
  :: (SOP.SListI xs, SOP.AllF (SOP.All Arbitrary) xs, SOP.AllF SOP.SListI xs)
  => Gen (SOP.SOP SOP.I xs)
sopArbitrary' = oneof (map SOP.hsequence $ SOP.apInjs_POP $ SOP.hcpure p arbitrary)
  where
    p :: Proxy Arbitrary
    p = Proxy


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


instance Arbitrary (URIRef Absolute) where
  arbitrary = URI <$> arbitrary
                  <*> arbitrary
                  <*> arbitrary
                  <*> arbitrary
                  <*> arbitrary


instance Arbitrary (URIRef Relative) where
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


instance Arbitrary URINormalizationOptions where
  arbitrary = URINormalizationOptions <$> arbitrary
                                      <*> arbitrary
                                      <*> arbitrary
                                      <*> arbitrary
                                      <*> arbitrary
                                      <*> arbitrary
                                      <*> arbitrary
                                      <*> arbitrary


instance Arbitrary SchemaError where
  arbitrary = sopArbitrary
  shrink = genericShrink


instance Arbitrary URIParseError where
  arbitrary = sopArbitrary
  shrink = genericShrink
