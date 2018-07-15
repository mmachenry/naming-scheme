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
  BApp function argument ->
    if isValue function
    then case function of
           BAbs body -> Ok (subst 0 argument body)
           _ -> Err "Cannot apply numberic value."
    else step function |> Result.andThen (\res->Ok (BApp res argument))
  BIfZero cond conseq alt ->
    if isValue cond
    then case cond of
           BZero -> Ok conseq
           BSucc _ -> Ok alt
           BAbs _ -> Err "Not a number in conditional."
           _ -> Err "Not a value."
    else step cond |> Result.andThen (\res->Ok (BIfZero res conseq alt))
  BZero -> Ok term
  BSucc term1 ->
    if isValue term1
    then case term1 of
           BAbs _ -> Err "Cannot apply succ to non-number."
           _ -> Ok term
    else Result.map BSucc (step term1)
  BPred term1 ->
    if isValue term1
    then case term1 of
           BAbs _ -> Err "Cannot apply pred to non-number."
           BSucc n -> Ok n
           BZero -> Err "Cannot apply pred to zero."
           _ -> Err "Not a value"
    else Result.map BPred (step term1)
  BFix term -> Ok (BApp term (BFix term))

isValue : BTerm -> Bool
isValue term = case term of
  BAbs _ -> True
  BZero -> True
  BSucc n -> isValue n
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
  BZero -> term
  BSucc term1 -> BSucc (subst depth replacement term1)
  BPred term1 -> BPred (subst depth replacement term1)
  BFix term1 -> BFix (subst depth replacement term1)

