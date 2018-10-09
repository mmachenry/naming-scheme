module DeBruijnTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import DeBruijn exposing (..)


suite : Test
suite =
  describe "The DeBruijn module"
    [ describe "DeBruijn.parse"
      [ test "parses λ0" <|
        \_ ->
          "λ0" |> DeBruijn.parse
            |> Expect.equal (Ok (BAbs (BVar 0)))
      , test "parses λ 0" <|
        \_ ->
          "λ 0"
            |> DeBruijn.parse
            |> Expect.equal (Ok (BAbs (BVar 0)))
      , test "parses succ zero" <|
        \_ ->
          "succ zero"
            |> DeBruijn.parse
            |> Expect.equal (Ok (BApp (BPrim Succ) (BPrim Zero)))
      ]
    , describe "DeBruijn.lex"
      [ test "lexes λ0" <|
        \_ ->
          "λ0"
            |> DeBruijn.lex
            |> Expect.equal (Ok [LexRes RLambda, LexVar 0])
      , test "lexes λ 0" <|
        \_ ->
          "λ 0"
            |> DeBruijn.lex
            |> Expect.equal (Ok [LexRes RLambda, LexVar 0])
      , test "lexes λ 0 with trailing space" <|
        \_ ->
          "λ 0 "
            |> DeBruijn.lex
            |> Expect.equal (Ok [LexRes RLambda, LexVar 0])
      , test "lexes succ zero" <|
        \_ ->
          "succ zero"
            |> DeBruijn.lex
            |> Expect.equal (Ok [LexPrim Succ, LexPrim Zero])
      ]
    ]
