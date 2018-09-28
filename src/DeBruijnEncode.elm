module DeBruijnEncode exposing (deBruijnEncode)

import LambdaCalculus exposing (Expr)
import DeBruijn exposing (BExpr)
import Operators exposing (Operator)

deBruijnEncode : List Identifier -> Expr -> Result String BExpr
encode idStack expr = case expr of
  Var ident ->
    case elemIndex ident idStack of
      Just i -> Ok (BVar i)
      Nothing ->
        case ident of
          "zero" -> Ok (BPrim "zero")
          "succ" -> Ok (BPrim "succ")
          "pred" -> Ok (BPrim "pred")
          "fix" -> Ok (BPrim "fix")
          _ -> Err ("Unbound identifier: " ++ ident)
  Abs ident body ->
    Result.map BAbs (encode (ident::idStack) body)
  Bind ident value body ->
    Result.map2 BBind (encode idStack value)
                      (encode (ident::idStack) body)
  App expr1 expr2 ->
    Result.map2 App (encode idStack expr1)
                    (encode idStack expr2)
  IfZero expr1 expr2 expr3 ->
    Result.map3 IfZero (encode idStack expr1)
                       (encode idStack expr2)
                       (encode idStack expr3)
  OpExpr op expr1 expr2 ->
    Result.map2 (OpExpr op) (encode idStack expr1)
                            (encode idStack expr2)
  BVar _ -> Ok expr
  BAbs _ -> Ok expr
  BBind _ _ -> Ok expr
  BPrim _ -> Ok expr

