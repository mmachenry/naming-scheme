module DeBruijn exposing (BTerm(..), deBruijnEncode)

import Parser exposing (..)
import Result exposing (Result)
import List.Extra exposing (elemIndex)

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

deBruijnEncode : List Ident -> Term -> Result String BTerm
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
  Zero -> Ok BZero
  Succ term -> Result.map BSucc (deBruijnEncode idStack term)
  Pred term -> Result.map BPred (deBruijnEncode idStack term)
  Fix term -> Result.map BFix (deBruijnEncode idStack term)

