module Main where

import Control.Monad.Trans.Except (runExceptT)
import Text.Parsec.Error (ParseError)

import Env (Val, EvalException)
import Interpreter (newEnv, evalExpr)
import Parser (buildAst)


main :: IO ()
main = putStrLn "Ok"

data ExecResult
    = ExecSuccess Val
    | ExecParseError ParseError
    | ExecEvalError EvalException    
          deriving (Show)        

exec :: String -> IO ExecResult
exec code = do
    case buildAst code of
        Left parseError -> return $ ExecParseError parseError
        Right ast       -> do
            env <- newEnv
            result <- runExceptT $ evalExpr env ast
            case result of
                Left evalException -> return $ ExecEvalError evalException
                Right val          -> return $ ExecSuccess val