module Operator exposing (Operator(..), toString)

type Operator = Add | Sub | Mul | Div | App

toString : Operator -> String
toString op = case op of
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "/"
  App -> "$"

