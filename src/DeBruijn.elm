module DeBruijn exposing (..)

import Parser exposing (..)
import Combine exposing (..)
import Combine.Num
import Result exposing (Result)
import Dict exposing (Dict)

type BTerm =
    BVar Int
  | BAbs BTerm
  | BBind BTerm BTerm
  | BApp BTerm BTerm
  | BIfZero BTerm BTerm BTerm
  | BPrim ReservedWord

type ReservedWord =
  RLambda | RLet | RIn | RIf | RThen | RElse | RZero |
  RSucc | RPred | RFix | ROpen | RClose

-- Parse

parseDeBruijn = buildParser lambdaCalcWords lambdaCalcVarRef

parseNamingScheme = buildParser namingSchemeWords namingSchemeVarRef

buildParser :
     (ReservedWord -> String)
  -> (Parser () BTerm)
  -> (String -> Result String BTerm)
buildParser rword parseVarRef =
  let bTerm : Parser () BTerm
      bTerm = chainl (whitespace $> BApp) (lazy (\_->bAtom))

      bAtom : Parser () BTerm
      bAtom =
            symbol ROpen *> lazy (\_->bTerm) <* symbol RClose
        <|> lazy (\_->bAbstraction)
        <|> lazy (\_->bBinding)
        <|> lazy (\_->bIfZero)
        <|> symbol RSucc $> BPrim RSucc
        <|> symbol RPred $> BPrim RPred
        <|> symbol RFix $> BPrim RFix
        <|> symbol RZero $> BPrim RZero
        <|> parseVarRef 

      bAbstraction : Parser () BTerm
      bAbstraction =
        BAbs <$> (symbol RLambda *> lazy (\_->bTerm))

      bBinding : Parser () BTerm
      bBinding =
        symbol RLet *> lazy (\_->bTerm) >>= \value->
          symbol RIn *> lazy (\_->bTerm) >>= \body->
            succeed (BBind value body)

      bIfZero : Parser () BTerm
      bIfZero =
        symbol RIf *> lazy (\_->bTerm) >>= \condition->
          symbol RThen *> lazy (\_->bTerm) >>= \consequent->
            symbol RElse *> lazy (\_->bTerm) >>= \alternate->
              succeed (BIfZero condition consequent alternate)

      symbol : ReservedWord -> Parser () String
      symbol r = whitespace *> string (rword r)

  in parseString bTerm

parseString : Parser () a -> String -> Result String a
parseString parser input =
  case parse parser input of
    Ok (_, stream, result) -> Ok result
    Err (_, stream, errors) -> Err (String.join " or " errors)


lambdaCalcWords : ReservedWord -> String
lambdaCalcWords r = case r of
  RLambda -> "λ"
  RLet -> "let"
  RIn -> "in"
  RIf -> "if"
  RThen -> "then"
  RElse -> "else"
  RZero -> "zero"
  RSucc -> "succ"
  RPred -> "pred"
  RFix -> "fix"
  ROpen -> "("
  RClose -> ")"

lambdaCalcVarRef = BVar <$> (whitespace *> Combine.Num.int <* whitespace)

namingSchemeWords : ReservedWord -> String
namingSchemeWords r = case r of
  RLambda -> "Proxy"
  RLet -> "Global"
  RIn -> "Decorator"
  RIf -> "Initializer"
  RThen -> "Factory"
  RElse -> "Bean"
  RZero -> "Observer"
  RSucc -> "Session"
  RPred -> "Prototype"
  RFix -> "Helper"
  ROpen -> "Parser"
  RClose -> "Adapter"

namingSchemeVarRef =
  many (string "Meta") >>= \metaStr->
    string "Object" >>= \_->
      succeed (BVar (List.length metaStr))

-- Print

toDeBruijnString : BTerm -> String
toDeBruijnString = buildPrinter lambdaCalcWords toString " "

toNamingSchemeString : BTerm -> String
toNamingSchemeString =
  buildPrinter namingSchemeWords (\n->String.repeat n "Meta" ++ "Object") ""

buildPrinter :
     (ReservedWord -> String)
  -> (Int -> String)
  -> String
  -> BTerm
  -> String
buildPrinter rword printVarRef seperator =
  let pTerm : BTerm -> String
      pTerm term = case term of
        BVar i -> printVarRef i
        BAbs body -> rword RLambda ++ pTerm body
        BBind value body ->
          String.join
            seperator
            [rword RLet, pTerm value, rword RIn, pTerm body]
        BApp term1 term2 -> pAppLeft term1 ++ seperator ++ pAtom term2
        BIfZero term1 term2 term3 ->
          String.join
            seperator
            ([rword RIf, pTerm term1,
              rword RThen, pTerm term2,
              rword RElse, pTerm term3])
        BPrim prim -> rword prim

      pAtom : BTerm -> String
      pAtom term = case term of
        BApp _ _ -> parens (pTerm term)
        _ -> pTerm term

      pAppLeft : BTerm -> String
      pAppLeft term = case term of
        BAbs _ -> parens (pTerm term)
        _ -> pTerm term

      parens : String -> String
      parens l = rword ROpen ++ l ++ rword RClose

  in pTerm
