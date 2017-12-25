module Ast where

data UnaryOp
    = UnaryPlus
    | UnaryMinus
        deriving (Eq, Show)

data BinaryOp
    = BinaryPow
    | BinaryMul
    | BinaryDiv
    | BinaryMod
    | BinaryAdd
    | BinarySub
    | BinaryAssign
        deriving (Eq, Show) 

data Expr
    = NumLiteralE Integer
    | IdentifierE String
    | ParensE Expr
    | PrefixOpE UnaryOp Expr
    | BinaryOpE BinaryOp Expr Expr
    | CallE Expr [Expr]
    | SubscriptE Expr Expr
        deriving (Eq, Show)
