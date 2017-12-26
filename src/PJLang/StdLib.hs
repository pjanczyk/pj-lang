module PJLang.StdLib (print, println) where

import Prelude hiding (print)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except

import PJLang.Env


print :: Env -> [Val] -> IOExceptEval Val
print _ [arg] = do
    let str = case arg of
            IntVal int       -> show int
            StringVal string -> string
            NativeFuncVal _  -> "<native function>"
            NullVal          -> "<null>"
    lift $ putStr str
    return NullVal

print _ _ = throwE $ EvalException $ "Function `print` expect 1 argument"


println :: Env -> [Val] -> IOExceptEval Val
println env args = do
    print env args
    lift $ putStrLn ""
    return NullVal