import Test.DocTest

main :: IO ()
main = doctest [
    "-isrc"
  , "-XOverloadedStrings"
  , "src/URI/ByteString.hs"
  ]
