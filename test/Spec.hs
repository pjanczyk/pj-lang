import Control.Monad (void)
import Test.HUnit
import qualified Test.PJLang.Parser (tests)
import qualified Test.PJLang.Interpreter (tests)

main :: IO ()
main = void $ runTestTT $ test [
        Test.PJLang.Parser.tests,
        Test.PJLang.Interpreter.tests
    ]
