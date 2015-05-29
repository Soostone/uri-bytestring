doctest:
	doctest -XOverloadedStrings -isrc -idist/build/autogen/ -optP-include -optPdist/build/autogen/cabal_macros.h src/URI/ByteString.hs

.PHONY: doctest
