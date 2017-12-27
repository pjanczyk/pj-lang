module PJLang.StdLib (print, printLine, readLine) where

import Prelude hiding (print)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

import PJLang.Env


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

print _env _ = throwE $ EvalException $ "Function `print` expect 1 argument"


printLine :: Env -> [Val] -> IOExceptEval Val
printLine env args = do
    print env args
    lift $ putStrLn ""
    return NullVal


readLine :: Env -> [Val] -> IOExceptEval Val
readLine _env [] = StringVal <$> lift getLine