module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax


int :: Parser Expr
int = do
  n <- integer
  return $ Float (fromInteger n)

floating :: Parser Expr
floating = do
  n <- float
  return $ Float n

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor
       where
        binary s f = Ex.Infix (reservedOp s >> return (BinOp f))
        table = [[binary "*" Times Ex.AssocLeft,
                binary "/" Divide Ex.AssocLeft]
                ,[binary "+" Plus Ex.AssocLeft,
                binary "-" Minus Ex.AssocLeft]]

variable :: Parser Expr
variable = do
  var <- identifier
  return $ Var var

definition :: Parser Expr
definition = do
  reserved "def"
  name <- identifier
  body <- expr
  reservedOp "="
  return $ Definition name body

function :: Parser Expr
function = do
  reserved "fun"
  args <- parens $ many variable
  reservedOp "->"
  body <- expr
  return $ Function args body

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many variable
  return $ Extern name args

call :: Parser Expr
call = do
  name <- expr
  args <- parens $ commaSep expr
  return $ Call name args

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try extern
      <|> try function
      <|> try call
      <|> variable
      <|> parens expr

datatype :: Parser Expr
datatype = do
   reserved "datatype"
   name <- identifier
   reservedOp "="
   cons <- constructor `sepBy1` reservedOp "|"
   return $ DataType name cons

constructor :: Parser Constructor
constructor = do
    name <- identifier
    cons <- optionMaybe (reserved "of" >> constructor_arity)
    let arity = case cons of
                          Nothing -> 0
                          Just a -> length a
    return $ Con name arity
    where
    constructor_arity = try (dot >> return [()]) <|> parens (many1 dot)
    dot = reservedOp "."

defn :: Parser Expr
defn = try extern
    <|> try datatype
    <|> try function
    <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel) "<stdin>"
