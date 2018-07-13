module Eval exposing (eval, Value(..))

import Parser exposing (..)
import Dict exposing (Dict)

type alias Environment = Dict Identifier Value

type Value =
    Closure Environment Term
  | Nat Int

eval : Environment -> Term -> Result String Value
eval env term = case term of
  Var ident -> Result.fromMaybe ("Unbound: " ++ ident) (Dict.get ident env)
  Abs ident body -> Ok (Closure env term)
  Bind ident val body ->
    eval env val
    |> Result.andThen (\value-> eval (Dict.insert ident value env) body)
  App term1 term2 ->
    eval env term1
    |> Result.andThen (\func->
       eval env term2
       |> Result.andThen (\arg-> 
          case func of
            Closure cEnv (Abs ident body) ->
              eval (Dict.insert ident arg cEnv) body
            _ -> Err "Malformed closure."))
  IfZero cond conseq alt ->
    eval env cond
    |> Result.andThen (\testVal ->
         case testVal of
           Nat n -> if n == 0
                    then eval env conseq
                    else eval env alt
           _ -> Err "Not a number in condition")
  Zero -> Ok (Nat 0)
  Succ term -> eval env term |> Result.andThen add1
  Pred term -> eval env term |> Result.andThen sub1
  Fix term -> eval env (App term (Fix term))

add1 : Value -> Result String Value
add1 x = case x of
  Nat n -> Ok (Nat (n + 1))
  _ -> Err "Cannot add to non number."

sub1 : Value -> Result String Value
sub1 x = case x of
  Nat n -> if n > 0
           then Ok (Nat (n-1))
           else Err "Cannot subtract from zero."
  _ -> Err "Cannot subtract from non number."
