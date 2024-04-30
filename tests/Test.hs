import Test.Tasty
import Common
import Hw3

main :: IO ()
main = runTests [ unit ]

unit :: Score -> TestTree
unit sc = testGroup "Unit" [
  mkTest
      simpleEval
      (NumE 8)
      8
      "simpleEval 1"
  , mkTest
      simpleEval
      (PlusE (NumE 3) (MinusE (NumE 2) (NumE 1)))
      4
      "simpleEval 2"
  , mkTest
      simpleEval
      (TimesE (TimesE (NumE 4) (NumE 6)) (PlusE (NumE 1) (NumE 2)))
      72
      "simpleEval 3"
  , mkTest
      simpleEval
      (MinusE (NumE (-3)) (PlusE (NumE 1) (NumE 2)))
      (-6)
      "simpleEval 4"
  , mkTest
      (opMaybe (+) (Just 3))
      (Just 5)
      (Just 8)
      "opMaybe 1"
  , mkTest
      (opMaybe (-) (Just 3))
      (Just 5)
      (Just (-2))
      "opMaybe 2"
  , mkTest
      (opMaybe (*) (Just 3))
      (Just 5)
      (Just 15)
      "opMaybe 3"
  , mkTest
      (opMaybe (+) (Just 3))
      Nothing
      Nothing
      "opMaybe 4"
  , mkTest
      (opMaybe (-) Nothing)
      (Just 3)
      Nothing
      "opMaybe 5"
  , mkTest
      (opMaybe (*) Nothing)
      Nothing
      Nothing
      "opMaybe 6"
  , mkTest
      (opMaybe (\x y -> x + y + 5) (Just 3))
      (Just 2)
      (Just 10)
      "opMaybe 7"
  , mkTest
      (opMaybe (\x y -> x + y + 5) Nothing)
      (Just 2)
      Nothing
      "opMaybe 8"
  , mkTest
      (varExprListEval [("x", 3)])
      (Var "x")
      (Just 3)
      "varExprListEval 1"
  , mkTest
      (varExprListEval [("x", 4), ("y", 7)])
      (PlusVE (Var "x") (MinusVE (NumVE 2) (Var "y")))
      (Just (-1))
      "varExprListEval 2"
  , mkTest
      (varExprListEval [("x", 4), ("y", 7)])
      (TimesVE (Var "z") (NumVE 3))
      Nothing
      "varExprListEval 3"
  , mkTest
      (varExprListEval [])
      (TimesVE (Var "x") (NumVE 3))
      Nothing
      "varExprListEval 4"
  , mkTest
      (varExprListEval [])
      (TimesVE (NumVE 7) (NumVE 3))
      (Just 21)
      "varExprListEval 5"
  , mkTest
      (varExprFunEval (\v -> if v == "x" then Just 3 else Nothing))
      (Var "x")
      (Just 3)
      "varExprFunEval 1"
  , mkTest
      (varExprFunEval (\v -> if v == "x" then Just 4 else (if v == "y" then Just 7 else Nothing)))
      (PlusVE (Var "x") (MinusVE (NumVE 2) (Var "y")))
      (Just (-1))
      "varExprFunEval 2"
  , mkTest
      (varExprFunEval (\v -> if v == "x" then Just 4 else (if v == "y" then Just 7 else Nothing)))
      (TimesVE (Var "z") (NumVE 3))
      Nothing
      "varExprFunEval 3"
  , mkTest
      (varExprFunEval (\v -> Nothing))
      (TimesVE (Var "x") (NumVE 3))
      Nothing
      "varExprFunEval 4"
  , mkTest
      (varExprFunEval (\v -> Nothing))
      (TimesVE (NumVE 7) (NumVE 3))
      (Just 21)
      "varExprFunEval 5"
  , mkTest
      show
      (NumVE 8)
      "8"
      "show 1"
  , mkTest
      show
      (TimesVE (NumVE 7) (NumVE 3))
      "(7 * 3)"
      "show 2"
  , mkTest
      show
      (PlusVE (NumVE 3) (MinusVE (NumVE 2) (NumVE 1)))
      "(3 + (2 - 1))"
      "show 3"
   , mkTest
      show
      (TimesVE (TimesVE (NumVE 4) (NumVE 6)) (PlusVE (NumVE 1) (NumVE 2)))
      "((4 * 6) * (1 + 2))"
      "show 4"
   , mkTest
      show
      (Var "x")
      "x"
      "show 5"
   , mkTest
      show
      (TimesVE (Var "x") (PlusVE (NumVE 1) (Var "y")))
      "(x * (1 + y))"
      "show 6"
   , mkTest
      ((==) (TimesVE (NumVE 7) (NumVE 3)))
      (TimesVE (NumVE 3) (NumVE 7))
      True
      "Eq 1"
   , mkTest
      ((==) (TimesVE (NumVE 7) (NumVE 3)))
      (TimesVE (NumVE 6) (NumVE 7))
      False
      "Eq 2"
   , mkTest
      ((==) (NumVE 2))
      (NumVE 3)
      False
      "Eq 3"
   , mkTest
      ((==) (Var "x"))
      (Var "x")
      True
      "Eq 4"
   , mkTest
      ((==) (Var "y"))
      (Var "x")
      True
      "Eq 5"
   , mkTest
      ((==) (TimesVE (NumVE 7) (NumVE 3)))
      (TimesVE (NumVE 7) (Var "x"))
      False
      "Eq 6"
   , mkTest
      (lookupInEnv "x")
      (extendEnv "x" 3 (emptyEnv :: ListEnv))
      (Just 3)
      "instance Env ListEnv 1"
   , mkTest
      (lookupInEnv "y")
      (extendEnv "x" 3 (emptyEnv :: ListEnv))
      Nothing
      "instance Env ListEnv 2"
   , mkTest
      (lookupInEnv "y")
      (extendEnv "x" 3 (extendEnv "y" 4 (emptyEnv :: ListEnv)))
      (Just 4)
      "instance Env ListEnv 3"
   , mkTest
      (lookupInEnv "x")
      (extendEnv "x" 3 (emptyEnv :: FunEnv))
      (Just 3)
      "instance Env FunEnv 1"
   , mkTest
      (lookupInEnv "y")
      (extendEnv "x" 3 (emptyEnv :: FunEnv))
      Nothing
      "instance Env FunEnv 2"
   , mkTest
      (lookupInEnv "y")
      (extendEnv "x" 3 (extendEnv "y" 4 (emptyEnv :: FunEnv)))
      (Just 4)
      "instance Env FunEnv 3"
   , mkTest
      (varExprEval (extendEnv "x" 3 (emptyEnv :: ListEnv)))
      (Var "x")
      (Just 3)
      "varExprEval ListEnv 1"
   , mkTest
      (varExprEval (extendEnv "x" 4 (extendEnv "y" 7 (emptyEnv :: ListEnv))))
      (PlusVE (Var "x") (MinusVE (NumVE 2) (Var "y")))
      (Just (-1))
      "varExprEval ListEnv 2"
   , mkTest
      (varExprEval (extendEnv "x" 3 (extendEnv "y" 7 (emptyEnv :: ListEnv))))
      (TimesVE (Var "z") (NumVE 3))
      Nothing
      "varExprEval ListEnv 3"
   , mkTest
      (varExprEval (emptyEnv :: ListEnv))
      (TimesVE (Var "x") (NumVE 3))
      Nothing
      "varExprEval ListEnv 4"
   , mkTest
      (varExprEval (emptyEnv :: ListEnv))
      (TimesVE (NumVE 7) (NumVE 3))
      (Just 21)
      "varExprEval ListEnv 5"
   , mkTest
      (varExprEval (extendEnv "x" 3 (emptyEnv :: FunEnv)))
      (Var "x")
      (Just 3)
      "varExprEval FunEnv 1"
   , mkTest
      (varExprEval (extendEnv "x" 4 (extendEnv "y" 7 (emptyEnv :: FunEnv))))
      (PlusVE (Var "x") (MinusVE (NumVE 2) (Var "y")))
      (Just (-1))
      "varExprEval FunEnv 2"
   , mkTest
      (varExprEval (extendEnv "x" 3 (extendEnv "y" 7 (emptyEnv :: FunEnv))))
      (TimesVE (Var "z") (NumVE 3))
      Nothing
      "varExprEval FunEnv 3"
   , mkTest
      (varExprEval (emptyEnv :: FunEnv))
      (TimesVE (Var "x") (NumVE 3))
      Nothing
      "varExprEval FunEnv 4"
   , mkTest
      (varExprEval (emptyEnv :: FunEnv))
      (TimesVE (NumVE 7) (NumVE 3))
      (Just 21)
      "varExprEval FunEnv 5"
   , mkTest
      (evalAll (emptyEnv :: ListEnv))
      [NumVE 30, NumVE 40, NumVE 20]
      [Just 30, Just 40, Just 20]
      "evalAll ListEnv 1"
   , mkTest
      (evalAll (emptyEnv :: ListEnv))
      [TimesVE (Var "z") (NumVE 3), TimesVE (NumVE 0) (NumVE 3)]
      [Nothing, Just 0]
      "evalAll ListEnv 2"
   , mkTest
      (evalAll (extendEnv "z" 3 (emptyEnv :: ListEnv)))
      [NumVE 0, MinusVE (NumVE 2) (NumVE 1), TimesVE (Var "z") (NumVE 3)]
      [Just 0, Just 1, Just 9]
      "evalAll ListEnv 3"
   , mkTest
      (evalAll (emptyEnv :: ListEnv))
      [TimesVE (NumVE 7) (NumVE 3), TimesVE (NumVE 3) (NumVE 7)]
      [Just 21, Just 21]
      "evalAll ListEnv 4"
   , mkTest
      (evalAll (extendEnv "z" 3 (emptyEnv :: ListEnv)))
      [Var "z", TimesVE (Var "z") (Var "z"), NumVE 1]
      [Just 3, Just 9, Just 1]
      "evalAll ListEnv 5"
   , mkTest
      (evalAll (extendEnv "z" 3 (emptyEnv :: ListEnv)))
      [NumVE 1, Var "x", Var "z"]
      [Just 1, Nothing, Just 3]
      "evalAll ListEnv 6"
   , mkTest
      (evalAll (emptyEnv :: FunEnv))
      [NumVE 30, NumVE 40, NumVE 20]
      [Just 30, Just 40, Just 20]
      "evalAll FunEnv 1"
   , mkTest
      (evalAll (emptyEnv :: FunEnv))
      [TimesVE (Var "z") (NumVE 3), TimesVE (NumVE 0) (NumVE 3)]
      [Nothing, Just 0]
      "evalAll FunEnv 2"
   , mkTest
      (evalAll (extendEnv "z" 3 (emptyEnv :: FunEnv)))
      [NumVE 0, MinusVE (NumVE 2) (NumVE 1), TimesVE (Var "z") (NumVE 3)]
      [Just 0, Just 1, Just 9]
      "evalAll FunEnv 3"
   , mkTest
      (evalAll (emptyEnv :: FunEnv))
      [TimesVE (NumVE 7) (NumVE 3), TimesVE (NumVE 3) (NumVE 7)]
      [Just 21, Just 21]
      "evalAll FunEnv 4"
   , mkTest
      (evalAll (extendEnv "z" 3 (emptyEnv :: FunEnv)))
      [Var "z", TimesVE (Var "z") (Var "z"), NumVE 1]
      [Just 3, Just 9, Just 1]
      "evalAll FunEnv 5"
   , mkTest
      (evalAll (extendEnv "z" 3 (emptyEnv :: FunEnv)))
      [NumVE 1, Var "x", Var "z"]
      [Just 1, Nothing, Just 3]
      "evalAll FunEnv 6"
   , mkTest
      (sumEval (emptyEnv :: ListEnv))
      [NumVE 30, NumVE 40, NumVE 20]
      (Just 90)
      "sumEval ListEnv 1"
   , mkTest
      (sumEval (emptyEnv :: ListEnv))
      [TimesVE (Var "z") (NumVE 3), TimesVE (NumVE 0) (NumVE 3)]
      Nothing
      "sumEval ListEnv 2"
   , mkTest
      (sumEval (extendEnv "z" 3 (emptyEnv :: ListEnv)))
      [NumVE 0, MinusVE (NumVE 2) (NumVE 1), TimesVE (Var "z") (NumVE 3)]
      (Just 10)
      "sumEval ListEnv 3"
   , mkTest
      (sumEval (emptyEnv :: ListEnv))
      [TimesVE (NumVE 7) (NumVE 3), TimesVE (NumVE 3) (NumVE 7)]
      (Just 42)
      "sumEval ListEnv 4"
   , mkTest
      (sumEval (extendEnv "z" 3 (emptyEnv :: ListEnv)))
      [Var "z", TimesVE (Var "z") (Var "z"), NumVE 1]
      (Just 13)
      "sumEval ListEnv 5"
   , mkTest
      (sumEval (extendEnv "z" 3 (emptyEnv :: ListEnv)))
      [NumVE 1, Var "x", Var "z"]
      Nothing
      "sumEval ListEnv 6"
   , mkTest
      (sumEval (emptyEnv :: FunEnv))
      [NumVE 30, NumVE 40, NumVE 20]
      (Just 90)
      "sumEval FunEnv 1"
   , mkTest
      (sumEval (emptyEnv :: FunEnv))
      [TimesVE (Var "z") (NumVE 3), TimesVE (NumVE 0) (NumVE 3)]
      Nothing
      "sumEval FunEnv 2"
   , mkTest
      (sumEval (extendEnv "z" 3 (emptyEnv :: FunEnv)))
      [NumVE 0, MinusVE (NumVE 2) (NumVE 1), TimesVE (Var "z") (NumVE 3)]
      (Just 10)
      "sumEval FunEnv 3"
   , mkTest
      (sumEval (emptyEnv :: FunEnv))
      [TimesVE (NumVE 7) (NumVE 3), TimesVE (NumVE 3) (NumVE 7)]
      (Just 42)
      "sumEval FunEnv 4"
   , mkTest
      (sumEval (extendEnv "z" 3 (emptyEnv :: FunEnv)))
      [Var "z", TimesVE (Var "z") (Var "z"), NumVE 1]
      (Just 13)
      "sumEval FunEnv 5"
   , mkTest
      (sumEval (extendEnv "z" 3 (emptyEnv :: FunEnv)))
      [NumVE 1, Var "x", Var "z"]
      Nothing
      "sumEval FunEnv 6"
  ]
  where
    mkTest :: (Show b, Eq b) => (a -> b) -> a -> b -> String -> TestTree
    mkTest = mkTest' sc

