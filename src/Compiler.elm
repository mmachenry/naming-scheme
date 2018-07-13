module Compiler exposing (compileToClass)

import DeBruijn exposing (..)
import String

-- TODO need to insert parenthesis minimally. See TAPL
-- Currently no parens are inserter at all and thus this is incorrect ATM.

-- TODO the encoding of let is wrong. We only need let,in,end, not = and I'm
-- not sure which Randy wants because his current code has Service for both
-- = and and end.
compileToClass : BTerm -> String
compileToClass term = case term of
  BVar i -> String.repeat i "Meta" ++ "Object"
  BAbs body -> "Proxy" ++ compileToClass body
  BBind val body ->
    "Global" ++ compileToClass val ++
    "Decorator" ++ compileToClass body ++ "Service"
  BApp term1 term2 -> compileToClass term1 ++ compileToClass term2
