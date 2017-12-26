module PJLang.Repl (runRepl) where

import Control.Monad.Trans.Except (runExceptT)
import System.IO (hFlush, stdout)
import Text.Parsec.Error (ParseError)

import PJLang.Env (Val, EvalException, Env)
import PJLang.Interpreter (newEnv, evalBlock)
import PJLang.Parser (buildAst)


runRepl :: IO ()
runRepl = do
    env <- newEnv
    sequence_ $ repeat $ do
        putStr "pjlang> "
        hFlush stdout
        code <- getLine
        result <- exec env code
        putStrLn $ show result


data ExecResult
    = ExecSuccess Val
    | ExecParseError ParseError
    | ExecEvalError EvalException    
            deriving (Show)        

exec :: Env -> String -> IO ExecResult
exec env code = do
    case buildAst code of
        Left parseError -> return $ ExecParseError parseError
        Right ast       -> do
            result <- runExceptT $ evalBlock env ast
            return $ case result of
                Left evalException -> ExecEvalError evalException
                Right val          -> ExecSuccess val