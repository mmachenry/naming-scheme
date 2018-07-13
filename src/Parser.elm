module Parser exposing (parsePCF, Term(..), Identifier)

import Combine exposing (..)

type alias Identifier = String

type Term =
    Var Identifier
  | Abs Identifier Term
  | Bind Identifier Term Term
  | App Term Term
  | IfZero Term Term Term
  | Zero
  | Succ Term
  | Pred Term
  | Fix Term

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
  <|> symbol "succ" *> map Succ (lazy (\_->atom))
  <|> symbol "pred" *> map Pred (lazy (\_->atom))
  <|> symbol "fix" *> map Fix (lazy (\_->atom))
  <|> symbol "0" $> Zero
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

ifZero : Parser () Term
ifZero =
  symbol "if0" *> whitespace *> lazy (\_->atom) >>= \condition->
    whitespace *> lazy (\_->atom) >>= \consequent->
      whitespace *> lazy (\_->atom) >>= \alternate->
        succeed (IfZero condition consequent alternate)

symbol : String -> Parser () String
symbol str = whitespace *> string str

identifier = (whitespace *> regex "[a-zA-Z0-9]+") >>= \word->
  if List.member word reservedWords
  then fail "Reserved word cannot be an identifier"
  else succeed word

reservedWords = [
  "lambda",
  "let",
  "=",
  "in",
  "end",
  "if0",
  "0",
  "succ",
  "pred",
  "fix"]
