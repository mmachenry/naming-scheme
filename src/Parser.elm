module Parser exposing (parsePCF, Term(..), Ident)

import Combine exposing (..)

type alias Ident = String

type Term =
    Var Ident
  | Abs Ident Term
  | Bind Ident Term Term
  | App Term Term

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
  <|> (Var <$> identifier)

abstraction : Parser () Term
abstraction =
  Abs <$>
    (symbol "lambda" *> identifier)
    <*> (symbol "." *> lazy (\_->term))

binding : Parser () Term
binding =
  symbol "let" *> identifier >>= \ident->
    symbol "=" *> lazy (\_->term) >>= \value->
      symbol "in" *> lazy (\_->term) >>= \body->
        succeed (Bind ident value body)

symbol : String -> Parser () String
symbol str = whitespace *> string str

identifier = (whitespace *> regex "[a-zA-Z0-9]+") >>= \word->
  if List.member word reservedWords
  then fail "Reserved word cannot be an identifier"
  else succeed word

reservedWords = ["lambda", "let", "=", "in", "end"]
