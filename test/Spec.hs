import Control.Monad (void)
import Test.HUnit
import qualified Test.PJLang.Parser (tests)

main :: IO ()
main = void $ runTestTT Test.PJLang.Parser.tests
