module L0 where

import Control.Monad

import Common

data Expr = Num Int | Read | Neg Expr | Add Expr Expr | Sub Expr Expr

instance Show Expr where
    show (Num n) = show n
    show Read = "(read)"
    show (Neg e) = "(- " ++ show e ++ " )"
    show (Add e1 e2) = "(+ " ++ show e1 ++ " " ++ show e2 ++ " )"

newtype Program = Program Expr

instance Show Program where
    show (Program e) = show e

-- TODO: Parser

-- ############################################################################
-- Interpreter
-- ############################################################################
interp :: Program -> IO Int
interp (Program e) = interpExpr e

interpExpr :: Expr -> IO Int
interpExpr (Num n) = pure n
interpExpr Read = do
    putStrLn "enter number: "
    read <$> getLine
interpExpr (Neg e) = do
    e' <- interpExpr e
    pure $ 0 - e'
interpExpr (Add e1 e2) = liftM2 (+) (interpExpr e1) (interpExpr e2)
interpExpr (Sub e1 e2) = liftM2 (-) (interpExpr e1) (interpExpr e2)

-- ############################################################################
-- Partial Evaluator
-- ############################################################################
peProgram :: Program -> Program
peProgram (Program e) = Program $ peExpr e

peAdd :: Expr -> Expr -> Expr
peAdd (Num n) (Num m) = Num (n + m)
peAdd e1 e2 = Add e1 e2

peSub :: Expr -> Expr -> Expr
peSub (Num n) (Num m) = Num (n + m)
peSub e1 e2 = Sub e1 e2

peExpr :: Expr -> Expr
peExpr (Num n) = Num n
peExpr Read = Read
peExpr (Neg e) = Neg $ peExpr e
peExpr (Add e1 e2) = peAdd (peExpr e1) (peExpr e2)
peExpr (Sub e1 e2) = peSub (peExpr e1) (peExpr e2)
