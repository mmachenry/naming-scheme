module ClassCompiler exposing (pcfToClass)

import PCFParser exposing (..)

pcfToClass ast = case ast of
  Var ident -> "var"
  Abs ident body -> "abs"
  Bind ident val body -> "bind"
  App exp1 exp2 -> pcfToClass exp1 ++ pcfToClass exp2
