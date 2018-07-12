module PCFParser exposing (parsePCF, Exp(..), Id)

import Combine exposing (..)

type alias Id = String

type Exp =
    Abs Id Exp
  | App Exp Exp
  | Let Id Exp Exp
  | Var Id

parsePCF : String -> Result String Exp
parsePCF input =
  case parse term input of
    Ok (_, stream, result) -> Ok result
    Err (_, stream, errors) -> Err (String.join " or " errors)

term : Parser () Exp
term =
      lazy (\_->application)
  <|> lazy (\_->abstraction)
  <|> lazy (\_->binding)

application : Parser () Exp
application =
      (App <$> lazy (\_->application) <*> lazy (\_->atom))
  <|> lazy (\_->atom)

atom : Parser () Exp
atom =
      symbol "(" *> lazy (\_->term) <* symbol ")"
  <|> (Var <$> id)

abstraction : Parser () Exp
abstraction =
  Abs <$>
    (symbol "lambda" *> id)
    <*> (symbol "." *> lazy (\_->term))

binding : Parser () Exp
binding =
  symbol "let" *> id
  >>= (\varid->symbol "=" *> lazy (\_->term)
  >>= (\bindExp->symbol "in" *> lazy (\_->term) <* symbol "end"
  >>= (\bodyExp->succeed (Let varid bindExp bodyExp))))

symbol : String -> Parser () String
symbol str = whitespace *> string str

id = whitespace *> regex "[a-zA-Z0-9]+"

