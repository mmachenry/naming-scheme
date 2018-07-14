module Eval exposing (eval, step)

import Parser exposing (..)

eval : Term -> Result String Int
eval term = case term of
  Num n -> Ok n
  Abs _ _ -> Err "Evaluated to a function"
  _ -> step term |> Result.andThen eval

step : Term -> Result String Term
step term = case term of
  Var ident -> Err ("Unbound identifier: " ++ ident)
  Abs ident body -> Ok term
  Bind ident val body -> Ok (subst ident val body)
  App term1 term2 ->
    case term1 of
      Num _ -> Err "Cannot apply a number."
      Abs ident body -> Ok (subst ident term2 body)
      _ -> step term1 |> Result.andThen (\res-> Ok (App res term2) )
  IfZero cond conseq alt ->
    case cond of
      Num 0 -> Ok conseq
      Num _ -> Ok alt
      Abs _ _ -> Err "Not a number in condition."
      _ -> step cond |> Result.andThen (\res->
             Ok (IfZero res conseq alt))
  Num n -> Ok (Num n)
  Succ term ->
    case term of
      Num n -> Ok (Num (n+1))
      Abs _ _ -> Err "Not a number in succ."
      _ -> Result.map Succ (step term)
  Pred term ->
    case term of
      Num 0 -> Err "Cannot subtract from 0."
      Num n -> Ok (Num (n-1))
      Abs _ _ -> Err "Not a number in succ."
      _ -> Result.map Pred (step term)
  Fix term -> Ok (App term (Fix term))

subst : Identifier -> Term -> Term -> Term
subst target replacement scope = case scope of
  Var ident ->
    if ident == target
    then replacement
    else scope
  Abs ident body ->
    if ident == target
    then scope
    else Abs ident (subst target replacement body)
  Bind ident value body ->
    Bind ident (subst target replacement value)
               (if ident == target
                then body
                else subst target replacement body)
  App term1 term2 ->
    App (subst target replacement term1)
        (subst target replacement term2)
  IfZero term1 term2 term3 ->
    IfZero (subst target replacement term1)
           (subst target replacement term2)
           (subst target replacement term3)
  Num n -> Num n
  Succ term -> Succ (subst target replacement term)
  Pred term -> Pred (subst target replacement term)
  Fix term -> Fix (subst target replacement term)

