module Operator exposing (..)

import Parser exposing (..)
import Parser.Extras exposing (..)

type Operator = Add | Sub | Mul

notFollowedBy : Parser a -> Parser ()
notFollowedBy p =
  oneOf [map (\_->True) (backtrackable p), succeed False]
  |> andThen (\b->if b then problem "found follow" else succeed ())

reservedOp : String -> Parser ()
reservedOp op = backtrackable (between spaces spaces (symbol op))

