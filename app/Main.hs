module Main where

import Control.Exception.Base (displayException)
import Control.Monad.Trans.Except (runExceptT)
import System.Environment (getArgs)
import System.IO (isEOF, hFlush, stdout)
import System.IO.Error (tryIOError)
import Text.Parsec.Error (ParseError)

import PJLang.Env (Val, EvalException(..), Env)
import PJLang.Interpreter (newEnv, eval)
import PJLang.Parser (buildAst)

data ExecResult
    = ExecSuccess Val
    | ExecParseError ParseError
    | ExecEvalError EvalException    
            deriving (Show)    

execCode :: Env -> String -> IO ExecResult
execCode env code = do
    case buildAst code of
        Left parseError -> return $ ExecParseError parseError
        Right ast       -> do
            result <- runExceptT $ eval env ast
            return $ case result of
                Left evalException -> ExecEvalError evalException
                Right val          -> ExecSuccess val

runRepl :: IO ()
runRepl = newEnv >>= loop
    where
        loop env = do
            putStr "pjlang> "
            hFlush stdout
            eof <- isEOF
            if eof
                then putStrLn ""
                else do
                    line <- getLine
                    result <- execCode env line
                    case result of
                        ExecSuccess val                   -> putStrLn (" " ++ show val)
                        ExecParseError parseError         -> putStrLn ("Invalid syntax: " ++ show parseError)
                        ExecEvalError (EvalException msg) -> putStrLn ("Runtime exception: " ++ msg)    
                    loop env

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-i"]     -> runRepl
        [filePath] -> do
            maybeContent <- tryIOError $ readFile filePath
            case maybeContent of
                Left err      -> putStrLn ("Failed to read file \"" ++ filePath ++ "\": " ++ displayException err)
                Right content -> do
                    env <- newEnv
                    result <- execCode env content
                    case result of
                        ExecSuccess _val                  -> return ()
                        ExecParseError parseError         -> putStrLn ("Invalid syntax: " ++ show parseError)
                        ExecEvalError (EvalException msg) -> putStrLn ("Runtime exception: " ++ msg)
        _          -> putStrLn ("PJ Language Interpreter v0.1\n" ++
                                "(c) 2017  Piotr Janczyk\n" ++
                                "Usage:\n" ++
                                "  pjlang -i     -- run REPL\n" ++
                                "  pjlang FILE   -- run program from script file")
