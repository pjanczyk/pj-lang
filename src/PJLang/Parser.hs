module PJLang.Parser (buildAst) where

import Control.Monad (void)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many)
import FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.String.Char (oneOf, digit, string, letter, char)
import Text.Parsec.String.Combinator (eof, manyTill, anyToken, many1, between, sepBy)
import Text.Parsec.String.Expr
import Text.Parsec.String.Parsec (parse, try)

import PJLang.Ast


whitespace :: Parser ()
whitespace = void $ many (oneOf [' ', '\n', '\t'])

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

integer :: Parser Integer
integer =  read <$> lexeme (many1 digit)

identifier :: Parser String
identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
  where
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> firstChar

symbol :: String -> Parser String
symbol s = lexeme $ string s

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

------------------------------------------------

expr :: Parser Expr
expr = buildExpressionParser table term
    where

baseTerm :: Parser Expr
baseTerm = identifierE <|> numLiteralE <|> parensE

term :: Parser Expr       
term = postfixE

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
    ]]
    where
        prefix op name = Prefix (PrefixOpE op <$ symbol name)
        binary op name assoc = Infix (BinaryOpE op <$ symbol name) assoc


numLiteralE :: Parser Expr
numLiteralE = NumLiteralE <$> integer

identifierE :: Parser Expr
identifierE = IdentifierE <$> identifier

parensE :: Parser Expr
parensE = parens expr

-- callE :: Parser Expr
-- callE = do
--     e <- baseTerm
--     maybeAddSuffix e
--     where
--         addSuffix callee = do
--             args <- between (symbol "(") (symbol ")") (expr `sepBy` (symbol ","))
--             maybeAddSuffix $ CallE callee args
        
--         maybeAddSuffix e = addSuffix e <|> return e


leftRecursive :: Parser Expr -> (Expr -> Parser Expr) -> Parser Expr
leftRecursive baseParser suffixParser = baseParser >>= tryAddSuffix
    where
        tryAddSuffix base = (suffixParser base >>= tryAddSuffix) <|> return base

postfixE :: Parser Expr
postfixE = leftRecursive baseTerm suffix
        where
            suffix e = callE e <|> subscriptE e
            callE e = CallE e <$> between (symbol "(") (symbol ")") (expr `sepBy` (symbol ","))
            subscriptE e = SubscriptE e <$> between (symbol "[") (symbol "]") expr

--------------------------------------------------------

buildAst :: String -> Either ParseError Expr
buildAst code = parseWithEof expr code
