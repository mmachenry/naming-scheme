module DeBruijn exposing (..)

import Parser exposing (..)
import Combine exposing (..)
import Combine.Num
import Result exposing (Result)
import List.Extra exposing (elemIndex)
import List exposing (concat, concatMap)
import Dict exposing (Dict)

type BTerm =
    BVar Int
  | BAbs BTerm
  | BBind BTerm BTerm
  | BApp BTerm BTerm
  | BIfZero BTerm BTerm BTerm
  | BZero
  | BSucc BTerm
  | BPred BTerm
  | BFix BTerm

type ReservedWord =
  RLambda | RLet | RIn | REnd | RIf | RThen | RElse | RZero |
  RSucc | RPred | RFix | ROpen | RClose

-- Parse

parseDeBruijn = buildParser lambdaCalcWords lambdaCalcVarRef

parseNamingScheme = buildParser namingSchemeWords namingSchemeVarRef

buildParser :
     (ReservedWord -> String)
  -> (Parser () BTerm)
  -> (String -> Result String BTerm)
buildParser rword parseVarRef =
  let parseString : String -> Result String BTerm
      parseString input =
        case parse bTerm input of
          Ok (_, stream, result) -> Ok result
          Err (_, stream, errors) -> Err (String.join " or " errors)

      bTerm : Parser () BTerm
      bTerm = chainl (whitespace $> BApp) (lazy (\_->bAtom))

      bAtom : Parser () BTerm
      bAtom =
            symbol ROpen *> lazy (\_->bTerm) <* symbol RClose
        <|> lazy (\_->bAbstraction)
        <|> lazy (\_->bBinding)
        <|> lazy (\_->bIfZero)
        <|> symbol RSucc *> map BSucc (lazy (\_->bAtom))
        <|> symbol RPred *> map BPred (lazy (\_->bAtom))
        <|> symbol RFix *> map BFix (lazy (\_->bAtom))
        <|> symbol RZero $> BZero
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

  in parseString

lambdaCalcWords : ReservedWord -> String
lambdaCalcWords r = case r of
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

lambdaCalcVarRef = BVar <$> (whitespace *> Combine.Num.int <* whitespace)

namingSchemeWords : ReservedWord -> String
namingSchemeWords r = case r of
  RLambda -> "Proxy"
  RLet -> "Global"
  RIn -> "Decorator"
  REnd -> "Service"
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
            [rword RLet, pTerm value, rword RIn, pTerm body, rword REnd]
        BApp term1 term2 -> pAppLeft term1 ++ seperator ++ pAtom term2
        BIfZero term1 term2 term3 ->
          String.join
            seperator
            ([rword RIf, pTerm term1,
              rword RThen, pTerm term2,
              rword RElse, pTerm term3])
        BZero -> rword RZero
        BSucc term1 -> rword RSucc ++ pAtom term1
        BPred term1 -> rword RPred ++ pAtom term1
        BFix term1 -> rword RFix ++ pAtom term1

      pAtom : BTerm -> String
      pAtom term = case term of
        BApp _ _ -> parens (pTerm term)
        -- BAbs _ -> parens (pTerm term)
        BSucc _ -> parens (pTerm term)
        BPred _ -> parens (pTerm term)
        BFix _ -> parens (pTerm term)
        _ -> pTerm term

      pAppLeft : BTerm -> String
      pAppLeft term = case term of
        BAbs _ -> parens (pTerm term)
        _ -> pTerm term

      parens : String -> String
      parens l = rword ROpen ++ l ++ rword RClose

  in pTerm
