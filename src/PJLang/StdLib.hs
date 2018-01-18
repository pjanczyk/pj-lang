module PJLang.StdLib (print, printLine, readLine, toString) where

import Prelude hiding (print)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

import PJLang.Env

-- | @`print`@ function. Accepts 1 argument of any type. Returns @null@.
print :: Env -> [Val] -> IOExceptEval Val
print _env [arg] = do
    let str = case arg of
            NullVal          -> "<null>"
            BoolVal bool     -> if bool then "true" else "false"
            IntVal int       -> show int
            StringVal string -> string
            NativeFuncVal _  -> "<native function>"
    lift $ putStr str
    return NullVal

print _env _ = throwE $ EvalException $ "Function `print` expects 1 argument"

-- | @`printLine`@ function. Accepts 1 argument of any type. Returns @null@.
printLine :: Env -> [Val] -> IOExceptEval Val
printLine env args = do
    print env args
    lift $ putStrLn ""
    return NullVal

-- | @`readLine`@ function. Accepts no arguments. Returns @string@.
readLine :: Env -> [Val] -> IOExceptEval Val
readLine _env [] = StringVal <$> lift getLine

-- | @`toString`@ function. Accepts 1 argument of any type. Returns @string@.
toString :: Env -> [Val] -> IOExceptEval Val
toString _env [arg] = return $ StringVal $ case arg of
    NullVal          -> "<null>"
    BoolVal bool     -> if bool then "true" else "false"
    IntVal int       -> show int
    StringVal string -> string
    NativeFuncVal _  -> "<native function>"

toString _env _ = throwE $ EvalException $ "Function `toString` expects 1 argument"
