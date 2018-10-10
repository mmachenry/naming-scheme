module DeBruijn exposing (
  BExpr(..),
  Primitive(..),
  ReservedWord(..),
  Lexeme(..),
  parse,
  lex,
  toString,
  buildPrinter,
  deBruijnWords
  )

import String
import Char
import List.Extra exposing (find)
import Debug
import Set
import Parser exposing (..)
import Parser.Expression exposing (..)
import Parser.Extras exposing (parens, between, many)
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
  | RApp

type Lexeme = LexVar Int | LexPrim Primitive | LexRes ReservedWord

parse : String -> Result String BExpr
parse input =
  case run expression input of
    Ok a -> Ok a
    Err deadEnds -> Err (Debug.toString deadEnds)

lex : String -> Result String (List Lexeme)
lex input =
  case run (lexer |. end) input of
    Ok a -> Ok a
    Err deadEnds -> Err (Debug.toString deadEnds)

lexer : Parser (List Lexeme)
lexer = many (oneOf (
     (map LexVar identifier)
  :: (map LexPrim primitive)
  :: (List.map mkKeywordLex wordMap)))

mkKeywordLex : (ReservedWord, String) -> Parser Lexeme
mkKeywordLex (rword, str) = succeed (LexRes rword) |. symbol str

expression : Parser BExpr
expression =
  oneOf [
    binding,
    ifZero,
    opExpr,
    term
    ]

abstraction : Parser BExpr
abstraction =
  succeed BAbs
    |. symbol "λ"
    |. spaces
    |= lazy (\_->expression)

binding : Parser BExpr
binding =
  succeed BBind
    |. keyword "let"
    |. spaces
    |= lazy (\_->expression)
    |. spaces
    |. keyword "in"
    |. spaces
    |= lazy (\_->expression)

ifZero : Parser BExpr
ifZero =
  succeed BIfZero
    |. keyword "if"
    |. spaces
    |= lazy (\_->expression)
    |. spaces
    |. keyword "then"
    |. spaces
    |= lazy (\_->expression)
    |. spaces
    |. keyword "else"
    |. spaces
    |= lazy (\_->expression)

identifier : Parser Int
identifier =
  getChompedString (chompWhile Char.isDigit)
  |> andThen (\numStr->
       case String.toInt numStr of
         Just i -> succeed i
         Nothing -> problem "Not an integer")

primitive : Parser Primitive
primitive =
  oneOf [
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
  abstraction,
  parens (lazy (\_->expression)),
  map BVar identifier,
  map BPrim primitive
  ]

-----------
-- Print
-----------

toString : BExpr -> String
toString = buildPrinter deBruijnWords String.fromInt " "

deBruijnWords : ReservedWord -> String
deBruijnWords rword = case find (\(r,s)->r==rword) wordMap of
  Just (_,s) -> s
  Nothing -> "ERROR"

wordMap : List (ReservedWord, String)
wordMap = [
  (RLambda, "λ"),
  (RLet, "let"),
  (RIn, "in"),
  (REnd, "end"),
  (RIf, "if"),
  (RThen, "then"),
  (RElse, "else"),
  (RZero, "zero"),
  (RSucc, "succ"),
  (RPred, "pred"),
  (RFix, "fix"),
  (ROpen, "("),
  (RClose, ")"),
  (RAdd, "+"),
  (RSub, "-"),
  (RMul, "*"),
  (RApp, "$")
  ]

buildPrinter :
     (ReservedWord -> String)
  -> (Int -> String)
  -> String
  -> BExpr
  -> String
buildPrinter rword printVarRef separator =
  let pExpr : BExpr -> String
      pExpr expr = case expr of
        BVar i -> printVarRef i
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
        App -> rword RApp

  in pExpr

