module DeBruijn exposing (
  BExpr(..),
  Primitive(..),
  ReservedWord(..),
  parse,
  toString,
  buildPrinter,
  deBruijnWords
  )

import String
import Debug
import Set
import Parser exposing (..)
import Parser.Expression exposing (..)
import Parser.Extras exposing (parens, between)
import Operator exposing (Operator(..), reservedOp, notFollowedBy)

type BExpr =
    BVar Int
  | BPrim Primitive
  | BAbs BExpr
  | BApp BExpr BExpr
  | BBind BExpr BExpr
  | BIfZero BExpr BExpr BExpr
  | BBinOp Operator BExpr BExpr

type Primitive = Fix | Zero | Succ | Pred

type ReservedWord =
    RLambda
  | RLet
  | RIn
  | REnd
  | RIf
  | RThen
  | RElse
  | RZero
  | RSucc
  | RPred
  | RFix
  | ROpen
  | RClose 
  | RAdd
  | RSub
  | RMul

parse : String -> Result String BExpr
parse input =
  case run expression input of
    Ok a -> Ok a
    Err deadEnds -> Err (String.join " or " (List.map Debug.toString deadEnds))

expression : Parser BExpr
expression =
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
    |= lazy (\_->expression)

binding : Parser BExpr
binding =
  succeed BBind
    |. keyword "let"
    |= lazy (\_->expression)
    |. keyword "in"
    |= lazy (\_->expression)

ifZero : Parser BExpr
ifZero =
  succeed BIfZero
    |. keyword "if"
    |= lazy (\_->expression)
    |. keyword "then"
    |= lazy (\_->expression)
    |. keyword "else"
    |= lazy (\_->expression)

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
    [ infixOperator (BBinOp Mul) (reservedOp "*") AssocLeft ],
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
  parens (lazy (\_->expression)),
  map BVar int,
  primitive
  ]

-----------
-- Print
-----------

toString : BExpr -> String
toString = buildPrinter deBruijnWords String.fromInt " "

deBruijnWords : ReservedWord -> String
deBruijnWords r = case r of
  RLambda -> "λ"
  RLet -> "let"
  RIn -> "in"
  REnd -> "end"
  RIf -> "if"
  RThen -> "then"
  RElse -> "else"
  RZero -> "zero"
  RSucc -> "succ"
  RPred -> "pred"
  RFix -> "fix"
  ROpen -> "("
  RClose -> ")"
  RAdd -> "+"
  RSub -> "-"
  RMul -> "*"

buildPrinter :
     (ReservedWord -> String)
  -> (Int -> String)
  -> String
  -> BExpr
  -> String
buildPrinter rword printVarRef separator =
  let pExpr : BExpr -> String
      pExpr expr = case expr of
        BVar i -> String.fromInt i
        BPrim prim -> case prim of
          Fix -> rword RFix
          Zero -> rword RZero
          Succ -> rword RSucc
          Pred -> rword RPred
        BAbs body -> rword RLambda ++ pExpr body
        BApp expr1 expr2 -> String.join separator [pAppLeft expr1, pAtom expr2]
        BBind value body ->
          String.join separator [rword RLet, pExpr value, rword RIn, pExpr body]
        BIfZero expr1 expr2 expr3 ->
          String.join separator [
            rword RIf, pExpr expr1,
            rword RThen, pExpr expr2,
            rword RElse, pExpr expr3]
        -- TODO use associativity to pretty print and drop some parens
        BBinOp op lhs rhs ->
          pParens (pExpr lhs) ++ pOperator op ++ pParens (pExpr rhs)

      -- TODO review what is an atom for bin ops
      pAtom : BExpr -> String
      pAtom expr = case expr of
        BApp _ _ -> pParens (pExpr expr)
        _ -> pExpr expr

      -- TODO review what is an app left for bin ops
      pAppLeft : BExpr -> String
      pAppLeft expr = case expr of
        BAbs _ -> pParens (pExpr expr)
        _ -> pExpr expr

      pParens : String -> String
      pParens l = rword ROpen ++ l ++ rword RClose 

      pOperator : Operator -> String
      pOperator op = case op of 
        Add -> rword RAdd
        Sub -> rword RSub
        Mul -> rword RMul

  in pExpr

