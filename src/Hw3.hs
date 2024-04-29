{- | CSE114A: Programming Assignment 3

     See the README for instructions.
 -}

module Hw3 where

import Prelude

-- | The `Expr` data type represents simple arithmetic expressions.
data Expr = PlusE Expr Expr
          | MinusE Expr Expr
          | TimesE Expr Expr
          | NumE Int

-- | `simpleEval` takes an expr `e`, evaluates it, and returns its value as an `Int`.
--
-- >>> simpleEval (NumE 8)
-- 8
--
-- >>> simpleEval (Plus (NumE 3) (Minus (NumE 2) (NumE 1)))
-- 4
--
-- >>> simpleEval (Times (Times (NumE 4) (NumE 6)) (Plus (NumE 1) (NumE 2)))
-- 72

simpleEval :: Expr -> Int
simpleEval e = error "TBD: simpleEval"





data VarExpr = PlusVE VarExpr VarExpr
             | MinusVE VarExpr VarExpr
             | TimesVE VarExpr VarExpr
             | NumVE Int
             | Var String

type ListEnv = [(String, Int)]

-- | `varExprListEval` takes an expression `e` of type `VarExpr`
--   and an environment `env` of type `ListEnv`
--   and evaluates `e` given `env`.
--   If `e` contains variables not bound in `env`,
--   `varExprListEval` returns `Nothing`.
--   Otherwise, `varExprListEval` returns the value of `e`, wrapped in `Just`.
--
-- >>> varExprListEval [("x", 3)] (Var "x")
-- Just 3
--
-- >>> varExprListEval [("x", 4), ("y", 7)] (PlusVE (Var "x") (MinusVE (NumVE 2) (Var "y")))
-- Just (-3)
--
-- >>> varExprListEval [("x", 4), ("y", 7)] (TimesVE (Var "z") (NumVE 3))
-- Nothing
--
-- >>> varExprListEval [] (TimesVE (Var "x") (NumVE 3))
-- Nothing
--
-- >>> varExprListEval [] (TimesVE (NumVE 7) (NumVE 3))
-- Just 21

varExprListEval :: VarExpr -> ListEnv -> Maybe Int
varExprListEval e env = error "TBD: varEval"





type FunEnv = String -> Maybe Int

-- | `varExprFunEval` takes an expression `e` of type `VarExpr`
--   and an environment `env` of type `FunEnv`
--   and evaluates `e` given `env`.
--   If `e` contains variables not bound in `env`,
--   `varExprFunEval` returns `Nothing`.
--   Otherwise, `varExprFunEval` returns the value of `e`, wrapped in `Just`.
--
-- >>> varExprFunEval (\v -> if v == "x" then 3 else Nothing) (Var "x")
-- Just 3
--
-- >>> varExprFunEval (\v -> if v == "x" then 4 else (if v == "y" then 7 else Nothing)) (PlusVE (Var "x") (MinusVE (NumVE 2) (Var "y")))
-- Just (-3)
--
-- >>> varExprFunEval (\v -> if v == "x" then 4 else (if v == "y" then 7 else Nothing)) (TimesVE (Var "z") (NumVE 3))
-- Nothing
--
-- >>> varExprFunEval (\v -> Nothing) (TimesVE (Var "x") (NumVE 3))
-- Nothing
--
-- >>> varExprFunEval (\v -> Nothing) (TimesVE (NumVE 7) (NumVE 3))
-- Just 21

varExprFunEval :: VarExpr -> FunEnv -> MaybeInt
varExprFunEval e env = error "TBD: varExprFunEval"
