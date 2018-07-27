module Parser exposing (..)

import Combine exposing (..)

type alias Identifier = String

type Term =
    Var Identifier
  | Abs Identifier Term
  | App Term Term
  | Bind Identifier Term Term
  | IfZero Term Term Term

parsePCF : String -> Result String Term
parsePCF input =
  case parse term input of
    Ok (_, stream, result) -> Ok result
    Err (_, stream, errors) -> Err (String.join " or " errors)

term : Parser () Term
term = chainl (whitespace $> App) (lazy (\_->atom))

atom : Parser () Term
atom =
      symbol "(" *> lazy (\_->term) <* symbol ")"
  <|> lazy (\_->abstraction)
  <|> lazy (\_->binding)
  <|> lazy (\_->ifZero)
  <|> (Var <$> identifier)

abstraction : Parser () Term
abstraction =
  Abs <$>
    ((symbol "λ" <|> symbol "lambda") *> identifier)
    <*> (symbol "." *> lazy (\_->term))

binding : Parser () Term
binding =
  symbol "let" *> identifier >>= \ident->
    symbol "=" *> lazy (\_->term) >>= \value->
      symbol "in" *> lazy (\_->term) >>= \body->
        succeed (Bind ident value body)

ifZero : Parser () Term
ifZero =
  symbol "if" *> lazy (\_->term) >>= \condition->
    symbol "then" *> lazy (\_->term) >>= \consequent->
      symbol "else" *> lazy (\_->term) >>= \alternate->
        succeed (IfZero condition consequent alternate)

symbol : String -> Parser () String
symbol str = whitespace *> string str

identifier = (whitespace *> regex "[a-zA-Z0-9]+") >>= \word->
  if List.member word reservedWords
  then fail "Reserved word cannot be an identifier"
  else succeed word
 
reservedWords = ["λ", "lambda", "let", "=", "in", "if", "then", "else"]
