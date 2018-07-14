module DeBruijn exposing (..)

import Parser exposing (..)
import Result exposing (Result)
import List.Extra exposing (elemIndex)
import List exposing (concat, concatMap)

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
