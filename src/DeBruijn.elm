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
  | BNum Int
  | BSucc BTerm
  | BPred BTerm
  | BFix BTerm

type ReservedWord =
  RLambda | RLet | RIn | REnd | RIf | RZero |
  RSucc | RPred | RFix | ROpen | RClose

deBruijnEncode : List Identifier -> Term -> Result String BTerm
deBruijnEncode idStack term = case term of
  Var ident ->
    case elemIndex ident idStack of
      Just i -> Ok (BVar i)
      Nothing -> Err ("Unbound identifier: " ++ ident)
  Abs ident body ->
    Result.map BAbs (deBruijnEncode (ident::idStack) body)
  Bind ident value body ->
    Result.map2 BBind (deBruijnEncode idStack value)
                      (deBruijnEncode (ident::idStack) body)
  App term1 term2 ->
    Result.map2 BApp (deBruijnEncode idStack term1)
                     (deBruijnEncode idStack term2)
  IfZero term1 term2 term3 ->
    Result.map3 BIfZero (deBruijnEncode idStack term1)
                        (deBruijnEncode idStack term2)
                        (deBruijnEncode idStack term3)
  Num n -> Ok (BNum n)
  Succ term -> Result.map BSucc (deBruijnEncode idStack term)
  Pred term -> Result.map BPred (deBruijnEncode idStack term)
  Fix term -> Result.map BFix (deBruijnEncode idStack term)

-- Parse

parseDeBruijn = buildParser lambdaCalcWords lambdaCalcVarRef

parseClass = buildParser classWords classVarRef

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
        <|> symbol RZero $> BNum 0
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
        symbol RIf *> whitespace *> lazy (\_->bAtom) >>= \condition->
          whitespace *> lazy (\_->bAtom) >>= \consequent->
            whitespace *> lazy (\_->bAtom) >>= \alternate->
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
  RZero -> "zero"
  RSucc -> "succ"
  RPred -> "pred"
  RFix -> "fix"
  ROpen -> "("
  RClose -> ")"

lambdaCalcVarRef = BVar <$> (whitespace *> Combine.Num.int <* whitespace)

classWords : ReservedWord -> String
classWords r = case r of
  RLambda -> "Proxy"
  RLet -> "Global"
  RIn -> "Decorator"
  REnd -> "Service"
  RIf -> "Initializer"
  RZero -> "Observer"
  RSucc -> "Session"
  RPred -> "Prototype"
  RFix -> "Helper"
  ROpen -> "Parser"
  RClose -> "Adapter"

classVarRef =
  many (string "Meta") >>= \metaStr->
    string "Object" >>= \_->
      succeed (BVar (List.length metaStr))

-- Print

pprBTerm : BTerm -> List String
pprBTerm term = case term of
  BVar i -> [toString i]
  BAbs body -> "λ" :: pprBTerm body
  BBind value body ->
    concat [["let"], pprBTerm value, ["in"], pprBTerm body, ["end"]]
  BApp term1 term2 -> concat [pprBAppLeft term1, pprBAtom term2]
  BIfZero term1 term2 term3 -> "if" :: concatMap pprBAtom [term1, term2, term3]
  BNum n -> if n == 0 then ["zero"] else ["undefined"]
  BSucc term1 -> "succ" :: pprBAtom term1
  BPred term1 -> "pred" :: pprBAtom term1
  BFix term1 -> "fix" :: pprBAtom term1

pprBAtom : BTerm -> List String
pprBAtom term = case term of
  BApp _ _ -> parens (pprBTerm term)
  BAbs _ -> parens (pprBTerm term)
  BSucc _ -> parens (pprBTerm term)
  BPred _ -> parens (pprBTerm term)
  BFix _ -> parens (pprBTerm term)
  _ -> pprBTerm term

parens : List String -> List String
parens l = concat [["("], l, [")"]]

pprBAppLeft : BTerm -> List String
pprBAppLeft term = case term of
  BAbs _ -> parens (pprBTerm term)
  _ -> pprBTerm term
