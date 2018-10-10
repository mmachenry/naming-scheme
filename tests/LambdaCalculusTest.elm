module LambdaCalculusTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import LambdaCalculus exposing (..)
import Operator exposing (..)


suite : Test
suite =
  describe "The LambdaCalculus module"
    [ describe "LambdaCalculus.parse"
      [ test "parses λx.x" <|
        \_ ->
          "λx.x" |> LambdaCalculus.parse
            |> Expect.equal (Ok (Abs "x" (Var "x")))
      , test "parses x x" <|
        \_ ->
          "x x"
          |> LambdaCalculus.parse
          |> Expect.equal (Ok (App (Var "x") (Var "x")))
      , test "parses fix λx.x" <|
        \_ ->
          "fix λx.x"
          |> LambdaCalculus.parse
          |> Expect.equal (Ok (App (Var "fix") (Abs "x" (Var "x"))))
      , test "parses factorial" <|
        \_ ->
          "let fact = fix λf.λn.if n then succ zero else n * f (pred n) in"
          ++ " fact $ succ $ succ $ succ zero"
          |> LambdaCalculus.parse
          |> Expect.equal
            (Ok (Bind "fact"
                (App (Var "fix")
                       (Abs "f" (Abs "n"
                         (IfZero
                           (Var "n")
                           (App (Var "succ") (Var "zero"))
                           (BinOp Mul (Var "n")
                             (App (Var "f") (App (Var "pred") (Var "n"))))))))
                (App (Var "fact")
                  (App (Var "succ")
                    (App (Var "succ")
                      (App (Var "succ")
                        (Var "zero")))))))
      ]
    ]
