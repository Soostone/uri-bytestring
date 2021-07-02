0.3.3.1
* Remove >= from cabal version. Thanks to [felixonmars](https://github.com/felixonmars).
* Conditionally drop dependency on semigroups library. Thanks to [felixonmars](https://github.com/felixonmars).

0.3.3.0
* Export more granular serializtion functions for things like the query string.

0.3.2.2
* Loosen dependencies

0.3.2.1
* Loosen upper bounds on template-haskell

0.3.2.0
* Only depend on the fail package when it is needed due to GHC version.

0.3.0.2
* Avoid using OverloadedStrings for Builder.

0.3.0.1
* Fix normalization bug where certain combination of options would fail to add a trailing slash.

0.3.0.0
* Add MonadFail instance.
* Correct haddock spelling mistake.

0.2.3.3
* Make buildable on GHC 8.2.1.

0.2.3.2
* Broaden dep on base.

0.2.3.1
* Add `relativeRef` quasi-quoter.

0.2.3.0
* Add `URI.ByteString.QQ` and the `uri` quasiquoter to be able to express statically known to be valid URIs, e.g. `example = [uri|http://www.example.com|]`. Thanks to [reactormonk](https://github.com/reactormonk).

0.2.2.1
* Drop dependency on derive in tests.

0.2.2.0
* Internally use attoparsec's numeric parser. Raise lower bounds on attoparsec.
* Allow blank fragments.

0.2.1.2
* Fixed bug introduced at 0.2.1.1 where RelativeRefs would fail to serialize their port numbers.

0.2.1.1
* Add URI normalization features.

0.2.1.0
* Widen dependency on base.

0.2.0.0
* Introduce URIRef, a GADT representation of absolute and relative URIs.

0.1.9.2
* Fix bug wher trailing ampersand in the query section would not parse.

0.1.9
* Fix type bug in serializeRelativeRef'

0.1.8
* Fix bug where uri-encoded paths would not decode when parsed.

0.1.7
* Add bytestring serialization functions. This is a common use case
  and exporting these prevents the user from directly depending on
  blaze-builder and re-implementing these functions in every application.

0.1.6
* Add Ord instances

0.1.5
* Fix serialization bug where userinfo was not including the @ separator.

0.1.4
* Bump attoparsec bounds

0.1.3
* Include test modules in distribution

0.1.2
* Add support for GHC 7.10

0.1.1
* Switch to blaze-bytestring for less contentious dependencies

0.1
* Add generic lenses (breaking field name changes).
* Add support for relative refs.
* Make Query instance of Generic, Typeable.

0.0.1

* Initial release.
