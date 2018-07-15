module Compiler exposing (compileToClass)

import DeBruijn exposing (..)
import String

compileToClass : BTerm -> String
compileToClass term = String.join "" (List.map alphaConvert (pprBTerm term))

alphaConvert : String -> String
alphaConvert word = case word of
  "λ" -> "Proxy"
  "fix" -> "Helper"
  "(" -> "Parser"
  ")" -> "Adapter"
  "succ" -> "Session"
  "pred" -> "Prototype"
  "if" -> "Initializer"
  "zero" -> "Observer"
  "let" -> "Global"
  "in" -> "Decorator"
  "end" -> "Service"
  str ->
    case String.toInt str of
      Ok n -> String.repeat n "Meta" ++ "Object"
      Err _ -> "!!!"
