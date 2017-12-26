module PJLang.Ast where

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
        deriving (Eq, Show) 

data Expr
    = NumLiteralE Integer
    | IdentifierE String
    | ParensE Expr
    | PrefixOpE UnaryOp Expr
    | BinaryOpE BinaryOp Expr Expr
    | CallE Expr [Expr]
    | SubscriptE Expr Expr
    | AssignE Expr Expr
        deriving (Eq, Show)

unaryOpToString :: UnaryOp -> String
unaryOpToString UnaryPlus  = "prefix +"
unaryOpToString UnaryMinus = "prefix -"

binaryOpToString :: BinaryOp -> String
binaryOpToString BinaryPow = "infix ^"
binaryOpToString BinaryMul = "infix *"
binaryOpToString BinaryDiv = "infix /"
binaryOpToString BinaryMod = "infix %"
binaryOpToString BinaryAdd = "infix +"
binaryOpToString BinarySub = "infix -"