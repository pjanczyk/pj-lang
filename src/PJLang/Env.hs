module PJLang.Env (
        Val(..),
        valType,
        Scope,
        Env(..),
        newScope,
        envInitial,
        getVar,
        setVar,
        scopeGetVar,
        scopeSetVar,
        EvalException(..),
        ExceptEval,
        IOExceptEval
    ) where

import Control.Monad.Trans.Except (Except, ExceptT)

import qualified PJLang.Ast as Ast (Expr)
import qualified PJLang.Util.MutableMap as MMap


data Val
    = NullVal
    | BoolVal Bool
    | IntVal Int
    | StringVal String
    | NativeFuncVal (Env -> [Val] -> IOExceptEval Val)
    | LambdaVal [Scope] [String] Ast.Expr

instance Show Val where
    show NullVal            = "null"
    show (BoolVal bool)     = if bool then "true" else "false"
    show (IntVal int)       = show int
    show (StringVal string) = "\"" ++ string ++ "\""
    show (NativeFuncVal _)  = "<native func>"
    show (LambdaVal _ _ _)  = "<lambda>"

valType :: Val -> String
valType NullVal           = "null"
valType (IntVal _)        = "int"
valType (BoolVal _)       = "bool"
valType (StringVal _)     = "string"
valType (NativeFuncVal _) = "native func"
valType (LambdaVal _ _ _) = "lambda"

newtype Scope = Scope (MMap.MutableMap String Val)

newScope :: IO Scope
newScope = Scope <$> MMap.empty

scopeHasVar :: Scope -> String -> IO Bool
scopeHasVar (Scope mmap) name = mmap `MMap.contains` name

scopeGetVar :: Scope -> String -> IO (Maybe Val)
scopeGetVar (Scope mmap) name = mmap `MMap.get` name

scopeSetVar :: Scope -> (String, Val) -> IO ()
scopeSetVar (Scope mmap) (name, val) = mmap `MMap.put` (name, val) 

newtype Env = Env [Scope]

envInitial :: IO Env
envInitial = Env . (:[]) <$> newScope

envFindVarScope :: Env -> String -> IO (Maybe Scope)
envFindVarScope (Env scopes) varName = findVarScope scopes
    where
        findVarScope []                   = return Nothing
        findVarScope (scope:parentScopes) = do
            defined <- scope `scopeHasVar` varName
            if defined
                then return $ Just scope
                else findVarScope parentScopes

envCurrentScope :: Env -> Scope
envCurrentScope (Env (scope:parentScopes)) = scope

getVar :: Env -> String -> IO (Maybe Val)
getVar env name = envFindVarScope env name >>= \x -> case x of
    Just scope -> scope `scopeGetVar` name
    Nothing    -> return Nothing

setVar :: Env -> (String, Val) -> IO ()
setVar env (name, val) = envFindVarScope env name >>= \x -> case x of
    Just scope -> scope `scopeSetVar` (name, val)
    Nothing    -> (envCurrentScope env) `scopeSetVar` (name, val)

newtype EvalException = EvalException String
    deriving (Show)

type ExceptEval a = Except EvalException a

type IOExceptEval a = ExceptT EvalException IO a
