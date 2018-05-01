module JSONParser ( json_parser )
where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import JSONTypes

lexer       = P.makeTokenParser emptyDef

parens          = P.parens lexer
brackets        = P.brackets lexer
braces          = P.braces lexer
commaSep        = P.commaSep lexer
whiteSpace      = P.whiteSpace lexer
symbol          = P.symbol lexer
identifier      = P.identifier lexer
integer         = P.integer lexer
stringLiteral   = P.stringLiteral lexer
colon           = P.colon lexer

json_parser :: Parser JValue
json_parser = do
    whiteSpace
    j_top <- (json_array_parser <|> json_obj_parser)
    return j_top

json_array_parser :: Parser JValue
json_array_parser = do
    j_vals <- brackets $ commaSep json_value_parser
    return $ JArray j_vals

json_obj_parser :: Parser JValue
json_obj_parser = do
    j_vals <- braces $ commaSep json_pair_parser -- a list of pairs
    return $ mkJObj j_vals

json_bool_parser :: Parser JValue
json_bool_parser = do
    bstr <- ( symbol "true" <|> symbol "false" )
    let
        bval = if bstr == "true" then True else False
    return $ JBool bval

json_string_parser :: Parser JValue
json_string_parser = do
  str <- between (char '\'') (char '\'') $ many stringChar
  return $ JString str

stringChar = try escapedQuote <|> noneOf "'"
    where escapedQuote = stringEscapeChar >> char '\''

stringEscapeChar = char '\\'

  -- symbol "\""
  -- str <- stringLiteral
  -- symbol "\""
  -- return $ JString str

json_number_parser :: Parser JValue
json_number_parser = do
  symbol "\""
  n <- integer
  symbol "\""
  return $ JNumber n

json_null_parser :: Parser JValue
json_null_parser = do
  symbol "\"null\""
  return $ JNull

json_pair_parser :: Parser JValue
json_pair_parser = do
    k <- stringLiteral
    colon
    v <- json_value_parser
    return $ mkJPair k v

json_value_parser :: Parser JValue
json_value_parser =
    json_array_parser <|>
    json_obj_parser <|>
    json_string_parser <|>
    json_number_parser <|>
    json_bool_parser <|>
    json_null_parser
