module PJLang.Ast where

type Op = String

data Expr
    = NullE
    | BoolE Bool
    | IntE Integer
    | StringE String
    | IdentifierE String
    | PrefixOpE Op Expr
    | InfixOpE Op Expr Expr
    | CallE Expr [Expr]
    | SubscriptE Expr Expr
    | BlockE [Expr]
    | IfElseE Expr Expr (Maybe Expr)
    | WhileE Expr Expr
        deriving (Eq, Show)
