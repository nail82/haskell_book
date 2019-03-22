module Hutton where

data Expr
    = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add ex1 ex2) = eval ex1 + eval ex2

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add ex1 ex2) = (printExpr ex1) ++ " + " ++ (printExpr ex2)
