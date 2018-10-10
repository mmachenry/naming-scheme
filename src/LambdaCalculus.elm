module LambdaCalculus exposing (Expr(..), Identifier, parse)

import Debug
import Set
import Parser exposing (..)
import Parser.Expression exposing (..)
import Parser.Extras exposing (parens, between)
import List.Extra exposing (elemIndex)
import Operator exposing (Operator(..), reservedOp, notFollowedBy)
import DeBruijn exposing (..)
import Debug

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
  case run expression input of
    Ok a -> Ok a
    Err deadEnds -> Err (Debug.toString deadEnds)

expression : Parser Expr
expression =
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
    |. spaces
    |= identifier
    |. spaces
    |. symbol "."
    |. spaces
    |= (lazy (\_->expression))

binding : Parser Expr
binding =
  succeed Bind
    |. keyword "let"
    |. spaces
    |= identifier
    |. spaces
    |. symbol "="
    |. spaces
    |= (lazy (\_->expression))
    |. spaces
    |. keyword "in"
    |. spaces
    |= (lazy (\_->expression))

ifZero : Parser Expr
ifZero =
  succeed IfZero
    |. keyword "if"
    |. spaces
    |= (lazy (\_->expression))
    |. spaces
    |. keyword "then"
    |. spaces
    |= (lazy (\_->expression))
    |. spaces
    |. keyword "else"
    |. spaces
    |= (lazy (\_->expression))

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
    parens (lazy (\_->expression)),
    variableReference
  ]

variableReference : Parser Expr
variableReference = map Var identifier

identifier : Parser String
identifier =
  variable {
    start = Char.isLower,
    inner = Char.isLower,
    reserved = Set.fromList ["let", "in", "if", "then", "else"]
    }

deBruijnEncode : List Identifier -> Expr -> Result String BExpr
deBruijnEncode idStack expr = case expr of
  Var ident ->
    case elemIndex ident idStack of
      Just i -> Ok (BVar i)
      Nothing ->
        case ident of
          "zero" -> Ok (BPrim Zero)
          "succ" -> Ok (BPrim Succ)
          "pred" -> Ok (BPrim Pred)
          "fix" -> Ok (BPrim Fix)
          _ -> Err ("Unbound identifier: " ++ ident)
  Abs ident body ->
    Result.map BAbs (deBruijnEncode (ident::idStack) body)
  App expr1 expr2 ->
    Result.map2 BApp (deBruijnEncode idStack expr1)
                     (deBruijnEncode idStack expr2)
  Bind ident value body ->
    Result.map2 BBind (deBruijnEncode idStack value)
                      (deBruijnEncode (ident::idStack) body)
  IfZero expr1 expr2 expr3 ->
    Result.map3 BIfZero (deBruijnEncode idStack expr1)
                        (deBruijnEncode idStack expr2)
                        (deBruijnEncode idStack expr3)
  BinOp op expr1 expr2 ->
    Result.map2 (BBinOp op) (deBruijnEncode idStack expr1)
                            (deBruijnEncode idStack expr2)

