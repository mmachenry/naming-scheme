module LambdaCalculus exposing (Expr(..), Identifier, parse)

import Debug exposing (toString)
import Set
import Parser exposing (..)
import Parser.Expression exposing (..)
import Parser.Extras exposing (parens, between)
import Operator exposing (Operator(..))

type alias Identifier = String

type Expr =
    Var Identifier
  | Abs Identifier Expr
  | App Expr Expr
  | Bind Identifier Expr Expr
  | IfZero Expr Expr Expr
  | BinOp Operator Expr Expr

parse : String -> Result String Expr
parse input =
  case run expr input of
    Ok a -> Ok a
    Err deadEnds -> Err (String.join " or " (List.map toString deadEnds))

expr : Parser Expr
expr =
  oneOf [
    abstraction,
    binding,
    ifZero,
    opExpr,
    term
    ]

abstraction : Parser Expr
abstraction =
  succeed Abs
    |. symbol "λ"
    |= identifier
    |. symbol "."
    |= (lazy (\_->expr))

binding : Parser Expr
binding =
  succeed Bind
    |. keyword "let"
    |= identifier
    |. symbol "="
    |= (lazy (\_->expr))
    |. keyword "in"
    |= (lazy (\_->expr))

ifZero : Parser Expr
ifZero =
  succeed IfZero
    |. keyword "if"
    |= (lazy (\_->expr))
    |. keyword "then"
    |= (lazy (\_->expr))
    |. keyword "else"
    |= (lazy (\_->expr))

opExpr : Parser Expr
opExpr = buildExpressionParser operators term

operators : OperatorTable Expr
operators =
  [ [ Infix (succeed App |. appOp) AssocLeft ],
    [ infixOperator (BinOp Mul) (reservedOp "*") AssocLeft,
      infixOperator (BinOp Div) (reservedOp "/") AssocLeft ],
    [ infixOperator (BinOp Add) (reservedOp "+") AssocLeft,
      infixOperator (BinOp Sub) (reservedOp "-") AssocLeft ],
    [ infixOperator App (reservedOp "$") AssocRight ]
  ]

appOp : Parser ()
appOp =
  let anyOperator = oneOf (List.map reservedOp ["*","/","+","-","$"])
  in backtrackable (spaces |. notFollowedBy anyOperator)

term : Parser Expr
term = oneOf [
    parens (lazy (\_->expr)),
    variableReference
  ]

variableReference : Parser Expr
variableReference = map Var identifier

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
