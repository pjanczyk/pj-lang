module PJLang.Ast where

-- | Text representation of an operator
type Op = String

-- | Abstract Syntax Tree
data Expr
    -- | @null@ literal
    = NullE                         
    -- | @true@ or @false@ literal
    | BoolE Bool                    
    -- | Integer literal
    | IntE Integer
    -- | String literal
    | StringE String           
    -- | Identifier (e.g. variable name)
    | IdentifierE String            
    -- | Prefix operator (@+@, @-@, @!@)
    | PrefixOpE Op Expr 
    -- | Infix operator (@+@, @-@, @*@, @/@, @%@, @^@,
    --       @==@, @!=@, @<@, @>@, @<=@, @>=@,
    --       @=@, @+=@, @-=@, @*=@, @/=@, @%=@, @^=@,
    --       @&&@, @||@)
    | InfixOpE Op Expr Expr 
    -- | Call expression, e.g. @`func(2, 3)`@
    | CallE                         
        Expr   -- callee
        [Expr] -- arguments
    -- | Subscript expression, e.g. @`array[0]`@
    | SubscriptE Expr Expr
    -- | Code block (list of statements), e.g. @`{ a; b; c }`@    
    | BlockE [Expr]
    -- | If-else statement. Usage: @IfElseE condition thenBody elseBody@
    | IfElseE
        Expr         -- condition
        Expr         -- then body
        (Maybe Expr) -- else body
    -- | While statement. Usage: @WhileE condition body@
    | WhileE
        Expr -- condition
        Expr -- body
    -- | Lambda function. Usage: @LambdaE paramenters body@
    | LambdaE
        [String] -- parameters
        Expr     -- body
        deriving (Eq, Show)
