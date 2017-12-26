module PJLang.Parser (buildAst) where

import Control.Monad (void)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many)
import FunctionsAndTypesForParsing (parseWithEof)
import Text.Parsec (ParseError)
import qualified Text.Parsec.Token as T
import Text.Parsec.String (Parser)
import Text.Parsec.String.Char (oneOf, digit, string, letter, char, alphaNum)
import Text.Parsec.String.Combinator (eof, manyTill, anyToken, many1, between, sepBy, sepEndBy, optionMaybe)
import Text.Parsec.String.Expr
import Text.Parsec.String.Parsec (parse, try)

import PJLang.Ast

tokenParser :: T.TokenParser ()
tokenParser = T.makeTokenParser $ T.LanguageDef
                { T.commentStart    = "/*"
                , T.commentEnd      = "*/"
                , T.commentLine     = "//"
                , T.nestedComments  = True
                , T.identStart      = letter <|> char '_'
                , T.identLetter     = alphaNum <|> oneOf "_'"
                , T.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , T.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , T.reservedOpNames = []
                , T.reservedNames   = []
                , T.caseSensitive   = True
                }

--------------------------------------------------
-- Token parsers
--------------------------------------------------

identifier :: Parser String
identifier = T.identifier tokenParser

keyword :: String -> Parser ()
keyword = T.reserved tokenParser

operator :: String -> Parser ()
operator = T.reservedOp tokenParser

charLiteral :: Parser Char
charLiteral = T.charLiteral tokenParser

stringLiteral :: Parser String
stringLiteral = T.stringLiteral tokenParser

natural :: Parser Integer
natural = T.natural tokenParser

symbol :: String -> Parser ()
symbol = void <$> T.symbol tokenParser

parens :: Parser a -> Parser a
parens p = between (symbol "(") (symbol ")") p

braces :: Parser a -> Parser a
braces p = between (symbol "{") (symbol "}") p

angles :: Parser a -> Parser a
angles p = between (symbol "<") (symbol ">") p

brackets :: Parser a -> Parser a
brackets p = between (symbol "[") (symbol "]") p

--------------------------------------------------
-- AST parsers
--------------------------------------------------

buildAst :: String -> Either ParseError Expr
buildAst code = parseWithEof stmtList code

blockE :: Parser Expr
blockE = braces stmtList

stmtList :: Parser Expr
stmtList = BlockE <$> (expr `sepEndBy` symbol ";")

expr :: Parser Expr
expr = buildExpressionParser table term

baseTerm :: Parser Expr
baseTerm = identifierE <|> numLiteralE <|> parensE

term :: Parser Expr
term = ifElseE <|> whileE <|> postfixE

table :: OperatorTable Expr
table =
    [[
        prefix UnaryPlus "+",
        prefix UnaryMinus "-"
    ], [
        binary BinaryPow "^" AssocRight
    ], [
        binary BinaryMul "*" AssocLeft,
        binary BinaryDiv "/" AssocLeft,
        binary BinaryMod "%" AssocLeft
    ], [
        binary BinaryAdd "+" AssocLeft,
        binary BinarySub "-" AssocLeft
    ], [
        Infix (AssignE <$ symbol "=") AssocRight  
    ]]
    where
        prefix op name = Prefix (PrefixOpE op <$ symbol name)
        binary op name assoc = Infix (BinaryOpE op <$ symbol name) assoc


numLiteralE :: Parser Expr
numLiteralE = NumLiteralE <$> natural

identifierE :: Parser Expr
identifierE = IdentifierE <$> identifier

parensE :: Parser Expr
parensE = parens expr

leftRecursive :: Parser Expr -> (Expr -> Parser Expr) -> Parser Expr
leftRecursive baseParser suffixParser = baseParser >>= tryAddSuffix
    where
        tryAddSuffix base = (suffixParser base >>= tryAddSuffix) <|> return base

postfixE :: Parser Expr
postfixE = leftRecursive baseTerm suffix
        where
            suffix e = callE e <|> subscriptE e
            callE e = CallE e <$> parens (expr `sepBy` (symbol ","))
            subscriptE e = SubscriptE e <$> brackets expr

ifElseE :: Parser Expr
ifElseE = IfElseE <$> cond <*> then' <*> optionMaybe else'
    where
        cond  = keyword "if" *> expr
        then' = (keyword "then" *> expr) <|> blockE
        else' = keyword "else" *> (expr <|> blockE)

whileE :: Parser Expr
whileE = WhileE <$> cond <*> body
    where
        cond = keyword "while" *> expr
        body = (keyword "do" *> expr) <|> blockE
