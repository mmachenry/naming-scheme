module DeBruijn exposing (BExpr(..), Primitive(..), parse)

import String
import List.Extra exposing (elemIndex)
import Debug exposing (toString)
import Set
import Parser exposing (..)
import Parser.Expression exposing (..)
import Parser.Extras exposing (parens, between)
import Operator exposing (Operator(..))

type BExpr =
    BVar Int
  | BPrim Primitive
  | BAbs BExpr
  | BApp BExpr BExpr
  | BBind BExpr BExpr
  | BIfZero BExpr BExpr BExpr
  | BBinOp Operator BExpr BExpr

type Primitive = Fix | Zero | Succ | Pred

parse : String -> Result String BExpr
parse input =
  case run expr input of
    Ok a -> Ok a
    Err deadEnds -> Err (String.join " or " (List.map toString deadEnds))

expr : Parser BExpr
expr =
  oneOf [
    abstraction,
    binding,
    ifZero,
    opExpr,
    term
    ]

abstraction : Parser BExpr
abstraction =
  succeed BAbs
    |. symbol "λ"
    |= (lazy (\_->expr))

binding : Parser BExpr
binding =
  succeed BBind
    |. keyword "let"
    |= (lazy (\_->expr))
    |. keyword "in"
    |= (lazy (\_->expr))

ifZero : Parser BExpr
ifZero =
  succeed BIfZero
    |. keyword "if"
    |= (lazy (\_->expr))
    |. keyword "then"
    |= (lazy (\_->expr))
    |. keyword "else"
    |= (lazy (\_->expr))

primitive : Parser BExpr
primitive =
  map BPrim <| oneOf [
    succeed Fix |. keyword "fix",
    succeed Zero |. keyword "zero",
    succeed Succ |. keyword "succ",
    succeed Pred |. keyword "pred"
    ]

--------------
-- Operators
--------------

opExpr : Parser BExpr
opExpr = buildExpressionParser operators term

operators : OperatorTable BExpr
operators =
  [ [ Infix (succeed BApp |. appOp) AssocLeft ],
    [ infixOperator (BBinOp Mul) (reservedOp "*") AssocLeft,
      infixOperator (BBinOp Div) (reservedOp "/") AssocLeft ],
    [ infixOperator (BBinOp Add) (reservedOp "+") AssocLeft,
      infixOperator (BBinOp Sub) (reservedOp "-") AssocLeft ],
    [ infixOperator BApp (reservedOp "$") AssocRight ]
  ]

appOp : Parser ()
appOp =
  let anyOperator = oneOf (List.map reservedOp ["*","/","+","-","$"])
  in backtrackable (spaces |. notFollowedBy anyOperator)

term : Parser BExpr
term = oneOf [
  parens (lazy (\_->expr)),
  map BVar int,
  primitive
  ]

notFollowedBy : Parser a -> Parser ()
notFollowedBy p =
  oneOf [map (\_->True) (backtrackable p), succeed False]
  |> andThen (\b->if b then problem "found follow" else succeed ())

reservedOp : String -> Parser ()
reservedOp op = backtrackable (between spaces spaces (symbol op))

-----------
-- Print
-----------

pExpr : BExpr -> String
pExpr expr = case expr of
  Var i -> i
  BPrim prim -> case prim of
    Fix -> "fix"
    Zero -> "zero"
    Succ -> "succ"
    Pred -> "pred"
  Abs i body -> "λ" ++ i ++ "." ++ pExpr body
  App expr1 expr2 -> String.join " " [pAppLeft expr1, pAtom expr2]
  Bind i value body ->
    String.join " " ["let", i, "=", pExpr value, "in", pExpr body]
  -- TODO use associativity to pretty print and drop some parens
  IfZero expr1 expr2 expr3 ->
    String.join " " [
      "if", pExpr expr1,
      "then", pExpr expr2,
      "else", pExpr expr3]
  BinOp op lhs rhs ->
    parens (pExpr lhs) ++ binOpToString op ++ parens (pExpr rhs)

-- TODO review what is an atom for bin ops
pAtom : Expr -> String
pAtom expr = case expr of
  App _ _ -> parens (pExpr expr)
  _ -> pExpr expr

-- TODO review what is an app left for bin ops
pAppLeft : Expr -> String
pAppLeft expr = case expr of
  Abs _ _ -> parens (pExpr expr)
  BAbs _ -> parens (pExpr expr)
  _ -> pExpr expr

parens : String -> String
parens l = "(" ++ l ++ ")"

