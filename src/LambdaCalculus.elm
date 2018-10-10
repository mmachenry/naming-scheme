module LambdaCalculus exposing (Expr(..), Identifier, parse)

import Debug
import Set
import Parser exposing (..)
import Parser.Expression exposing (..)
import Parser.Extras exposing (parens, between)
import Operator exposing (Operator(..), reservedOp, notFollowedBy)

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
    Err deadEnds -> Err (String.join " or " (List.map Debug.toString deadEnds))

expr : Parser Expr
expr =
  oneOf [
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
    |. spaces -- TODO why is this needed here and not elsewhere?
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
    [ infixOperator (BinOp Mul) (reservedOp "*") AssocLeft ],
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
    abstraction,
    parens (lazy (\_->expr)),
    variableReference
  ]

variableReference : Parser Expr
variableReference = map Var identifier

identifier : Parser String
identifier =
  succeed identity
    |. spaces
    |= variable {
         start = Char.isLower,
         inner = Char.isLower,
         reserved = Set.fromList ["let", "in", "if", "then", "else"]
         }
    |. spaces

