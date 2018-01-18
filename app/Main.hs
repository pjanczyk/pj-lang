module Main where

import Control.Exception.Base (displayException)
import System.Environment (getArgs)
import System.IO (isEOF, hFlush, stdout)
import System.IO.Error (tryIOError)

import PJLang.Env (EvalException(..))
import PJLang.Interpreter (ExecResult(..), newEnv, evalCode)


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
                    result <- evalCode env line
                    case result of
                        ExecSuccess val                   -> putStrLn (":: " ++ show val)
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
                    result <- evalCode env content
                    case result of
                        ExecSuccess _val                  -> return ()
                        ExecParseError parseError         -> putStrLn ("Invalid syntax: " ++ show parseError)
                        ExecEvalError (EvalException msg) -> putStrLn ("Runtime exception: " ++ msg)
        _          -> putStrLn ("PJ Language Interpreter v0.1\n" ++
                                "(c) 2017  Piotr Janczyk\n" ++
                                "Usage:\n" ++
                                "  pjlang -i     -- run REPL\n" ++
                                "  pjlang FILE   -- run program from script file")
