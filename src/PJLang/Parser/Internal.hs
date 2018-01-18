module PJLang.Parser.Internal where

import Control.Monad (void)
import Control.Applicative ((<$>), (<$), (<*>), (<*), (*>), (<|>), many)
import Data.Char (ord)
import FunctionsAndTypesForParsing (parseWithEof)
import Text.Parsec (ParseError)
import qualified Text.Parsec.Token as T
import Text.Parsec.String (Parser)
import Text.Parsec.String.Char (oneOf, digit, string, letter, char, alphaNum)
import Text.Parsec.String.Combinator (eof, manyTill, anyToken, many1, between, sepBy, sepEndBy, optionMaybe)
import Text.Parsec.String.Expr (Operator(Infix, Prefix), Assoc(AssocLeft, AssocRight), buildExpressionParser)
import Text.Parsec.String.Parsec (parse, try)

import PJLang.Ast

buildAst :: String -> Either ParseError Expr
buildAst code = parseWithEof stmtList code

--------------------------------------------------
-- Token parsers
--------------------------------------------------

tokenParser :: T.TokenParser ()
tokenParser = T.makeTokenParser $
    T.LanguageDef {
        T.commentStart    = "/*",
        T.commentEnd      = "*/",
        T.commentLine     = "//",
        T.nestedComments  = True,
        T.identStart      = letter <|> char '_',
        T.identLetter     = alphaNum <|> oneOf "_'",
        T.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~",
        T.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~",
        T.reservedOpNames = [],
        T.reservedNames   = ["if", "else", "then", "while", "do", "null", "true", "false"],
        T.caseSensitive   = True
    }

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
-- Helper functions
--------------------------------------------------

manyFoldl :: Parser Expr -> (Expr -> Parser Expr) -> Parser Expr
manyFoldl baseParser suffixParser = baseParser >>= tryAddSuffix
    where
        tryAddSuffix base = (suffixParser base >>= tryAddSuffix) <|> return base

--------------------------------------------------
-- AST parsers
--------------------------------------------------

nullE :: Parser Expr
nullE = NullE <$ keyword "null"

boolE :: Parser Expr
boolE = (BoolE True <$ keyword "true") <|> (BoolE False <$ keyword "false")

intE :: Parser Expr
intE = IntE <$> (natural <|> (toInteger <$> ord <$> charLiteral))

stringE :: Parser Expr
stringE = StringE <$> stringLiteral

identifierE :: Parser Expr
identifierE = IdentifierE <$> identifier

parensE :: Parser Expr
parensE = parens expr

baseE :: Parser Expr
baseE = nullE <|> boolE <|> intE <|> stringE <|> identifierE <|> parensE

postfixE :: Parser Expr
postfixE = baseE `manyFoldl` suffix
    where
        suffix e = callE e <|> subscriptE e
        callE e = CallE e <$> parens (expr `sepBy` symbol ",")
        subscriptE e = SubscriptE e <$> brackets expr

expr :: Parser Expr
expr = buildExpressionParser table (blockE <|> ifElseE <|> whileE <|> lambdaE <|> postfixE)
    where
        table =
            [[
                mkPrefix "+",
                mkPrefix "-"
            ], [
                mkInfix "^"  AssocRight
            ], [
                mkInfix "*"  AssocLeft,
                mkInfix "/"  AssocLeft,
                mkInfix "%"  AssocLeft
            ], [
                mkInfix "+"  AssocLeft,
                mkInfix "-"  AssocLeft
            ], [
                mkInfix "="  AssocRight,
                mkInfix "^=" AssocRight,
                mkInfix "*=" AssocRight,
                mkInfix "/=" AssocRight,
                mkInfix "%=" AssocRight,   
                mkInfix "+=" AssocRight,
                mkInfix "-=" AssocRight
            ], [
                mkInfix "==" AssocLeft,
                mkInfix "!=" AssocLeft,
                mkInfix "<"  AssocLeft,
                mkInfix ">"  AssocLeft,
                mkInfix "<=" AssocLeft,
                mkInfix ">=" AssocLeft
            ]]
        mkPrefix sym       = Prefix (PrefixOpE sym <$ operator sym)
        mkInfix  sym assoc = Infix  (InfixOpE sym  <$ operator sym) assoc

stmtList :: Parser Expr
stmtList = BlockE <$> (expr `sepEndBy` symbol ";")

blockE :: Parser Expr
blockE = braces stmtList

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

lambdaE :: Parser Expr
lambdaE = LambdaE <$> params <*> body
    where
        params = try $ (singleParam <|> multipleParams <|> noParams) <* keyword "->"
        noParams = return []
        singleParam = (: []) <$> identifier
        multipleParams = parens (identifier `sepBy` symbol ",")
        body = expr <|> blockE
