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
