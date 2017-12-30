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
    | CallE
        Expr   -- callee
        [Expr] -- arguments
    | SubscriptE Expr Expr
    | BlockE [Expr]
    | IfElseE
        Expr         -- condition
        Expr         -- then body
        (Maybe Expr) -- else body
    | WhileE
        Expr -- condition
        Expr -- body
    | FuncE
        [String] -- parameters
        Expr     -- body
        deriving (Eq, Show)
