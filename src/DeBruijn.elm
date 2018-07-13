module DeBruijn exposing (BTerm, deBruijnEncode)

import Parser exposing (..)
import Result exposing (Result)
import List.Extra exposing (elemIndex)

type BTerm =
    BVar Int
  | BAbs BTerm
  | BBind BTerm BTerm
  | BApp BTerm BTerm

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

