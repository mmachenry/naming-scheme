module Eval exposing (..)

import DeBruijn exposing (..)

eval : BTerm -> Result String BTerm
eval term =
  if isValue term
  then Ok term
  else step term |> Result.andThen eval

step : BTerm -> Result String BTerm
step term = case term of
  BVar _ -> Err "Reached unsubstituded variable reference."
  BAbs _ -> Ok term
  BBind value scope -> Ok (subst 0 value scope)
  BApp function argument -> case function of
    BAbs body -> Ok (subst 0 argument body)
    BPrim prim -> case prim of
      RSucc -> if isValue argument
               then Ok term
               else step argument |> Result.andThen (\res->
                                       Ok (BApp (BPrim RSucc) res))
      RPred -> if isValue argument
               then case argument of
                      BApp (BPrim RSucc) oneLess -> Ok oneLess
                      _ -> Err "Error applying pred"
               else step argument |> Result.andThen (\res->
                                       Ok (BApp (BPrim RPred) res))
      RFix -> Ok (BApp argument (BApp (BPrim RFix) argument))
      _ -> Err "Cannot apply numeric."
    _ -> step function |> Result.andThen (\res->Ok (BApp res argument))
  BIfZero cond conseq alt ->
    if isValue cond
    then case cond of
           BPrim RZero -> Ok conseq
           BApp (BPrim RSucc) _ -> Ok alt
           _ -> Err "Not a number in conditional."
    else step cond |> Result.andThen (\res->Ok (BIfZero res conseq alt))
  BPrim _ -> Ok term

isValue : BTerm -> Bool
isValue term = case term of
  BAbs _ -> True
  BPrim RZero -> True
  BApp (BPrim RSucc) n -> isValue n
  _ -> False

subst : Int -> BTerm -> BTerm -> BTerm
subst depth replacement term = case term of
  BVar n -> if n == depth then replacement else term
  BAbs body -> BAbs (subst (depth+1) replacement body)
  BBind value body ->
    BBind (subst depth replacement value)
          (subst (depth+1) replacement body)
  BApp term1 term2 ->
    BApp (subst depth replacement term1)
         (subst depth replacement term2)
  BIfZero term1 term2 term3 ->
    BIfZero (subst depth replacement term1)
            (subst depth replacement term2)
            (subst depth replacement term3)
  BPrim _ -> term

