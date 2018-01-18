module Test.PJLang.Interpreter (tests) where

import Data.Either.Combinators (fromRight')
import Test.HUnit (Test, test, assertEqual, assertFailure, (~:), (~?=))

import PJLang.Parser (buildAst)
import PJLang.Interpreter (ExecResult(..), newEnv, evalCode)
import PJLang.Env (Val(..), EvalException(..))


assertEvalSuccess :: String -> Val -> Test
assertEvalSuccess code expected = test $ do
    env <- newEnv
    result <- evalCode env code
    case result of
        ExecSuccess val -> assertEqual "" expected val
        otherwise       -> assertFailure (show otherwise)

assertEvalException :: String -> Test
assertEvalException code = test $ do
    env <- newEnv
    result <- evalCode env code
    case result of
        ExecEvalError _ -> return ()
        otherwise       -> assertFailure (show otherwise)


tests :: Test
tests = "Interpreter tests" ~: test [
         assertEvalSuccess "2 + 2" (IntVal 4),
         assertEvalSuccess "2 + 2 * 2" (IntVal 6),
         assertEvalSuccess "\"aaa\" + \"bb\"" (StringVal "aaabb"),
         assertEvalException "\"aaa\" + 5",
         assertEvalSuccess "(-> 1)()" (IntVal 1),      
         assertEvalSuccess "(x -> x + 1)(2)" (IntVal 3),  
         assertEvalSuccess "((a, b) -> a * b)(2, 3)" (IntVal 6),
         assertEvalSuccess "if True then 1" (IntVal 1),
         assertEvalSuccess "if False then 1" NullVal,
         assertEvalException "if 1 then 1",
         assertEvalSuccess "while False { }" NullVal
    ]