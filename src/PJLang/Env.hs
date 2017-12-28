module PJLang.Env where

import Control.Monad.Trans.Except (Except, ExceptT)

import qualified PJLang.Util.MutableMap as MMap


data Val
    = NullVal
    | BoolVal Bool
    | IntVal Int
    | StringVal String
    | NativeFuncVal (Env -> [Val] -> IOExceptEval Val) 

instance Show Val where
    show NullVal            = "null"
    show (BoolVal bool)     = if bool then "true" else "false"
    show (IntVal int)       = show int
    show (StringVal string) = "\"" ++ string ++ "\""
    show (NativeFuncVal _)  = "<native func>"

valType :: Val -> String
valType NullVal           = "null"
valType (IntVal _)        = "int"
valType (BoolVal _)       = "bool"
valType (StringVal _)     = "string"
valType (NativeFuncVal _) = "native func"

type Scope = MMap.MutableMap String Val

empty :: IO Scope
empty = MMap.empty

isVarDefined :: Scope -> String -> IO Bool
isVarDefined scope name = scope `MMap.contains` name

getVar :: Scope -> String -> IO (Maybe Val)
getVar scope name = scope `MMap.get` name

setVar :: Scope -> String -> Val -> IO ()
setVar scope name val = scope `MMap.put` (name, val) 

type Env = Scope

newtype EvalException = EvalException String
    deriving (Show)

type ExceptEval a = Except EvalException a

type IOExceptEval a = ExceptT EvalException IO a