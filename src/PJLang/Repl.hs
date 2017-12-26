module PJLang.Repl (runRepl) where

import Control.Monad.Trans.Except (runExceptT)
import System.IO (hFlush, stdout)
import Text.Parsec.Error (ParseError)

import PJLang.Env (Val, EvalException)
import PJLang.Interpreter (newEnv, evalExpr)
import PJLang.Parser (buildAst)


runRepl :: IO ()
runRepl = sequence_ $ repeat $ do
    putStr "pjlang> "
    hFlush stdout
    code <- getLine
    result <- exec code
    putStrLn $ show result


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