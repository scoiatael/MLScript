module Raw.Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Raw.Lexer
import Raw.Syntax


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
  reserved "let"
  name <- identifier
  reservedOp "="
  body <- factor
  return $ Definition name body

function :: Parser Expr
function = do
  reserved "fun"
  args <- many variable
  reservedOp "->"
  body <- factor
  return $ Function args body

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many variable
  return $ Extern name args

call :: Parser Expr
call = do
  name <- try variable <|> parens factor
  args <- try ( parens  arguments) <|> arguments
  return $ Call name args
  where
  arguments = factor `sepBy1` reservedOp ","

switch :: Parser Expr
switch = do
  reserved "case"
  value_expr <- expr
  reservedOp ":"
  datatype_name <- identifier
  reserved "of"
  optional $ reservedOp "|"
  switch_exprs <- switchExpr `sepBy1` reservedOp "|"
  return $ Switch value_expr datatype_name switch_exprs

switchExpr :: Parser SwitchExpr
switchExpr = do
  name <- identifier
  vars <- many identifier
  reservedOp "->"
  body <- factor
  return $ SwitchExpr name vars body

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try extern
      <|> try function
      <|> try switch
      <|> try call
      <|> variable
      <|> parens expr

datatype :: Parser Expr
datatype = do
   reserved "datatype"
   name <- identifier
   reservedOp "="
   cons <- constructor `sepBy1` reservedOp "|"
   return $ Datatype (name, cons)

constructor :: Parser Constructor
constructor = do
    name <- identifier
    cons <- optionMaybe (reserved "of" >> constructor_arity)
    let arity = case cons of
                          Nothing -> 0
                          Just a -> length a
    return $ Constructor name arity
    where
    constructor_arity = try (dot >> return [()]) <|> parens (many1 dot)
    dot = reservedOp "."

defn :: Parser Expr
defn = try extern
    <|> try datatype
    <|> try definition
    <|> factor

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = defn `sepBy1` optional (reservedOp ";")

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel) "<stdin>"
