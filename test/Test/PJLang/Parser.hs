module Test.PJLang.Parser (tests) where

import Data.Either (either)
import FunctionsAndTypesForParsing (parseWithEof)
import PJLang.Ast (Expr(..))
import qualified PJLang.Parser.Internal as PI
import Test.HUnit (Test, test, (~:), (~?=))
import Text.Parsec.String (Parser)


parseMaybe :: Parser a -> String -> Maybe a
parseMaybe parser input = either (const Nothing) Just $ parseWithEof parser input

tests :: Test
tests = "Parser tests" ~: test [
        parseMaybe PI.expr "var123"     ~?= Just (IdentifierE "var123"),
        parseMaybe PI.expr "123var"     ~?= Nothing,
        parseMaybe PI.expr "1337"       ~?= Just (IntE 1337),
        parseMaybe PI.expr "0xFA"       ~?= Just (IntE 250),
        parseMaybe PI.expr "true"       ~?= Just (BoolE True),
        parseMaybe PI.expr "false"      ~?= Just (BoolE False),
        parseMaybe PI.expr "null"       ~?= Just NullE,
        parseMaybe PI.expr "\"aa bb\""  ~?= Just (StringE "aa bb"),
        parseMaybe PI.expr "\" \\\" \"" ~?= Just (StringE " \" "),  
        parseMaybe PI.expr "a + 5"      ~?= Just (InfixOpE "+" (IdentifierE "a") (IntE 5)),
        parseMaybe PI.expr "a +++ 5"    ~?= Nothing,
        parseMaybe PI.expr "2 + 2 * 2"  ~?= Just (
            InfixOpE "+"
                     (IntE 2)
                     (InfixOpE "*" (IntE 2) (IntE 2))
        ),
        parseMaybe PI.expr "+1 + -2" ~?= Just (
            InfixOpE "+"
                     (PrefixOpE "+" (IntE 1))
                     (PrefixOpE "-" (IntE 2))
        ),     
        parseMaybe PI.expr "x = { a; b }" ~?= Just (
            InfixOpE "="
                     (IdentifierE "x")
                     (BlockE [IdentifierE "a", IdentifierE "b"])
        ),
        parseMaybe PI.expr "if x then 0 else 1" ~?= Just (
            IfElseE (IdentifierE "x")
                    (IntE 0)
                    (Just $ IntE 1)
        ),
        parseMaybe PI.expr "if x { a; b }" ~?= Just (
            IfElseE (IdentifierE "x")
                    (BlockE [
                        IdentifierE "a",
                        IdentifierE "b"
                    ])
                    Nothing
        ),
        parseMaybe PI.expr "while x do y" ~?= Just (
            WhileE (IdentifierE "x")
                   (IdentifierE "y")
        ),
        parseMaybe PI.expr "while x { y }" ~?= Just (
            WhileE (IdentifierE "x")
                   (BlockE [IdentifierE "y"])
        ),
        parseMaybe PI.expr "-> 1 + 1" ~?= Just (
            LambdaE []
                    (InfixOpE "+" (IntE 1) (IntE 1))
        ),
        parseMaybe PI.expr "x -> x" ~?= Just (
            LambdaE ["x"]
                    (IdentifierE "x")
        ),
        parseMaybe PI.expr "(a, b) -> a * b" ~?= Just (
            LambdaE ["a", "b"]
                    (InfixOpE "*" (IdentifierE "a") (IdentifierE "b"))
        )
    ]
