module Eval exposing (..)

import DeBruijn exposing (..)
import Operators exposing (Operator(..))

eval : BExpr -> Result String BExpr
eval term =
  if isValue term
  then Ok term
  else step term |> Result.andThen eval

step : BExpr -> Result String BExpr
step term = case term of
  BVar _ -> Err "Reached unsubstituded variable reference."
  BPrim _ -> Ok term
  BAbs _ -> Ok term
  BApp function argument -> case function of
    BAbs body -> Ok (subst 0 argument body)
    BPrim prim -> case prim of
      Succ -> if isValue argument
               then Ok term
               else step argument |> Result.andThen (\res->
                                       Ok (BApp (BPrim Succ) res))
      Pred -> if isValue argument
               then case argument of
                      BApp (BPrim Succ) oneLess -> Ok oneLess
                      _ -> Err "Error applying pred"
               else step argument |> Result.andThen (\res->
                                       Ok (BApp (BPrim Pred) res))
      Fix -> Ok (BApp argument (BApp (BPrim Fix) argument))
      _ -> Err "Cannot apply numeric."
    _ -> step function |> Result.andThen (\res->Ok (BApp res argument))
  BBind value scope -> Ok (subst 0 value scope)
  BIfZero cond conseq alt ->
    if isValue cond
    then case cond of
           BPrim Zero -> Ok conseq
           BApp (BPrim Succ) _ -> Ok alt
           _ -> Err "Not a number in conditional."
    else step cond |> Result.andThen (\res->Ok (BIfZero res conseq alt))
  BBinOp op lhs rhs -> Ok term -- TODO this is punting

isValue : BExpr -> Bool
isValue term = case term of
  BAbs _ -> True
  BPrim Zero -> True
  BApp (BPrim Succ) n -> isValue n
  _ -> False

subst : Int -> BExpr -> BExpr -> BExpr
subst depth replacement term = case term of
  BVar n -> if n == depth then replacement else term
  BPrim _ -> term
  BAbs body -> BAbs (subst (depth+1) replacement body)
  BApp term1 term2 ->
    BApp (subst depth replacement term1)
         (subst depth replacement term2)
  BBind value body ->
    BBind (subst depth replacement value)
          (subst (depth+1) replacement body)
  BIfZero term1 term2 term3 ->
    BIfZero (subst depth replacement term1)
            (subst depth replacement term2)
            (subst depth replacement term3)
  BBinOp op lhs rhs ->
    BBinOp op (subst depth replacement lhs)
              (subst depth replacement rhs)

