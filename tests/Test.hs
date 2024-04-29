import Test.Tasty
import Common
import Hw3

main :: IO ()
main = runTests [ unit ]

unit :: Score -> TestTree
unit sc = testGroup "Unit" [
  mkTest
      (\x -> x)
      "foo"
      "foo"
      "test 1"
  ]
  where
    mkTest :: (Show b, Eq b) => (a -> b) -> a -> b -> String -> TestTree
    mkTest = mkTest' sc
