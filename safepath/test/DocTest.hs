import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "-XOverloadedStrings", "src/Data/Path/Internal.hs"]
