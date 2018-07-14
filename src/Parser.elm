module Parser exposing (..)

import Combine exposing (..)

type alias Identifier = String

type Term =
    Var Identifier
  | Abs Identifier Term
  | Bind Identifier Term Term
  | App Term Term
  | IfZero Term Term Term
  | Num Int
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
  <|> symbol "0" $> Num 0
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
  "lambda", "let", "=", "in", "end",
  "if0", "0", "succ", "pred", "fix"]

-----------
-- Print --
-----------

lambdaStr = "λ"

pprTerm : Term -> String
pprTerm term = case term of
  Var ident -> ident
  Abs ident body ->
    lambdaStr ++ ident ++ "." ++ pprTerm body
  Bind ident value body ->
    "let " ++ ident ++ " = " ++ pprTerm value ++
    " in " ++ pprTerm body ++ " end"
  App term1 term2 ->
    pprLeftApp term1 ++ " " ++ pprAtom term2
  IfZero cond conseq alt ->
       "if0 " ++ pprAtom cond
    ++ " " ++ pprAtom conseq
    ++ " " ++ pprAtom alt
  Num n -> toString n
  Succ term1 -> "succ " ++ pprAtom term1
  Pred term1 -> "pred " ++ pprAtom term1
  Fix term1 -> "fix " ++ pprAtom term1

pprLeftApp : Term -> String
pprLeftApp term = case term of
  Abs _ _ -> parens (pprTerm term)
  _ -> pprTerm term

pprAtom : Term -> String
pprAtom term = case term of
  Abs _ _ -> parens (pprTerm term)
  App _ _ -> parens (pprTerm term)
  Succ _ -> parens (pprTerm term)
  Pred _ -> parens (pprTerm term)
  Fix _ -> parens (pprTerm term)
  _ -> pprTerm term

parens str = "(" ++ str ++ ")"

