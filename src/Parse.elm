module Parse exposing (Expr(..), BinOp(..), Identifier, parse)

import Debug exposing (toString)
import Set
import Parser exposing (..)
import Parser.Expression exposing (..)
import Parser.Extras exposing (parens, between)

type alias Identifier = String

type Expr =
    Var Identifier
  | Abs Identifier Expr
  | App Expr Expr
  | Bind Identifier Expr Expr
  | IfZero Expr Expr Expr
  | OpExpr BinOp Expr Expr
  | BVar Int
  | BAbs Expr
  | BBind Expr Expr

type BinOp = Add | Sub | Mul | Div

parse : Bool -> String -> Result String Expr
parse deBruijn input =
  case run (expr deBruijn) input of
    Ok a -> Ok a
    Err deadEnds -> Err (String.join " or " (List.map toString deadEnds))

expr : Bool -> Parser Expr
expr deBruijn =
  oneOf [
    abstraction deBruijn,
    binding deBruijn,
    ifZero deBruijn,
    opExpr deBruijn,
    term deBruijn
  ]

abstraction : Bool -> Parser Expr
abstraction deBruijn =
  if deBruijn
  then succeed BAbs
         |. symbol "λ"
         |= (lazy (\_->expr deBruijn))
  else succeed Abs
         |. symbol "λ"
         |= identifier
         |. symbol "."
         |= (lazy (\_->expr deBruijn))

binding : Bool -> Parser Expr
binding deBruijn =
  if deBruijn
  then succeed BBind
         |. keyword "let"
         |= (lazy (\_->expr deBruijn))
         |. keyword "in"
         |= (lazy (\_->expr deBruijn))
  else succeed Bind
         |. keyword "let"
         |= identifier
         |. symbol "="
         |= (lazy (\_->expr deBruijn))
         |. keyword "in"
         |= (lazy (\_->expr deBruijn))

ifZero : Bool -> Parser Expr
ifZero deBruijn =
  succeed IfZero
    |. keyword "if"
    |= (lazy (\_->expr deBruijn))
    |. keyword "then"
    |= (lazy (\_->expr deBruijn))
    |. keyword "else"
    |= (lazy (\_->expr deBruijn))

opExpr : Bool -> Parser Expr
opExpr deBruijn = buildExpressionParser operators (term deBruijn)

operators : OperatorTable Expr
operators =
  [ [ Infix (succeed App |. appOp) AssocLeft ],
    [ infixOperator (OpExpr Mul) (reservedOp "*") AssocLeft,
      infixOperator (OpExpr Div) (reservedOp "/") AssocLeft ],
    [ infixOperator (OpExpr Add) (reservedOp "+") AssocLeft,
      infixOperator (OpExpr Sub) (reservedOp "-") AssocLeft ],
    [ infixOperator App (reservedOp "$") AssocRight ]
  ]

appOp : Parser ()
appOp =
  let anyOperator = oneOf (List.map reservedOp ["*","/","+","-","$"])
  in backtrackable (spaces |. notFollowedBy anyOperator)

term : Bool -> Parser Expr
term deBruijn = oneOf [
    parens (lazy (\_->expr deBruijn)),
    variableReference deBruijn
  ]

variableReference : Bool -> Parser Expr
variableReference deBruijn =
  if deBruijn
  then map BVar int
  else map Var identifier

identifier : Parser String
identifier = variable {
  start = Char.isLower,
  inner = Char.isLower,
  reserved = Set.fromList ["let", "in", "if", "then", "else"]
  }

notFollowedBy : Parser a -> Parser ()
notFollowedBy p =
  oneOf [map (\_->True) (backtrackable p), succeed False]
  |> andThen (\b->if b then problem "found follow" else succeed ())

reservedOp : String -> Parser ()
reservedOp op = backtrackable (between spaces spaces (symbol op))
