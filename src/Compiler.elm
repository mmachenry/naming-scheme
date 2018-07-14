module Compiler exposing (compileToClass)

import DeBruijn exposing (..)
import String

-- TODO need to insert parenthesis minimally. See TAPL
-- Currently no parens are inserter at all and thus this is incorrect ATM.

-- TODO the encoding of let is wrong. We only need let,in,end, not = and I'm
-- not sure which Randy wants because his current code has Service for both
-- = and and end.

compileToClass : BTerm -> String
compileToClass term = String.join "" (List.map alphaConvert (pprBTerm term))

alphaConvert : String -> String
alphaConvert word = case word of
  -- "[variable] ": "/(Meta)*Object/",
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
  str -> case String.toInt str of
           Ok n -> String.repeat n "Meta" ++ "Object"
           Err _ -> "!!!"
