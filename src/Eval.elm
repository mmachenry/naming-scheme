module Eval exposing (..)

import DeBruijn exposing (..)

eval : BTerm -> Result String BTerm
eval term = case term of
  BNum _ -> Ok term
  BAbs _ -> Ok term
  _ -> step term |> Result.andThen eval

step : BTerm -> Result String BTerm
step term = case term of
  BVar _ -> Err "Reached unsubstituded variable reference."
  BAbs _ -> Ok term
  BBind value scope -> Ok (subst 0 value scope)
  BApp function argument ->
    case function of
      BNum _ -> Err "Cannot apply numberic value."
      BAbs body -> Ok (subst 0 argument body)
      _ -> step function |> Result.andThen (\res->Ok (BApp res argument))
  BIfZero cond conseq alt ->
    case cond of
      BNum 0 -> Ok conseq
      BNum _ -> Ok alt
      BAbs _ -> Err "Not a number in conditional."
      _ -> step cond |> Result.andThen (\res->Ok (BIfZero res conseq alt))
  BNum n -> Ok term
  BSucc term ->
    case term of
      BNum n -> Ok (BNum (n+1))
      BAbs _ -> Err "Not a number in succ."
      _ -> Result.map BSucc (step term)
  BPred term ->
    case term of
      BNum 0 -> Err "Cannot subtract from 0."
      BNum n -> Ok (BNum (n-1))
      BAbs _ -> Err "Not a number in succ."
      _ -> Result.map BPred (step term)
  BFix term -> Ok (BApp term (BFix term))

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
  BNum n -> term
  BSucc term1 -> BSucc (subst depth replacement term1)
  BPred term1 -> BPred (subst depth replacement term1)
  BFix term1 -> BFix (subst depth replacement term1)

