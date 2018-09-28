module DeBruijnEncode exposing (deBruijnEncode)

import List.Extra exposing (elemIndex)
import LambdaCalculus exposing (Expr(..), Identifier)
import DeBruijn exposing (BExpr(..), Primitive(..))
import Operator exposing (Operator)

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

