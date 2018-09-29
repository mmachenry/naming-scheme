module NamingScheme exposing (..)

import List.Extra exposing (dropWhile, takeWhile, find)
import DeBruijn exposing (BExpr, parse)

parse : String -> Result String BExpr
parse input =
  String.toList input
  |> toWords
  |> List.map fromNamingSchemeWord
  |> String.join " "
  |> DeBruijn.parse

toWords : List Char -> List String
toWords chars = case chars of
  [] -> []
  (firstLetter::otherLetters) ->
     String.fromList (firstLetter::(takeWhile Char.isLower otherLetters))
     :: (toWords (dropWhile Char.isLower otherLetters))

-- TODO handle error by propogating to parser Result
toNamingSchemeWord : String -> String
toNamingSchemeWord word = case find (\(l,n)->l==word) wordMap of
  Just (_,n) -> n
  Nothing -> "ERROR"

-- TODO handle error by propogating to parser Result
fromNamingSchemeWord : String -> String
fromNamingSchemeWord word = case find (\(l,n)->n==word) wordMap of
  Just (l,_) -> l
  Nothing -> "ERROR"

wordMap : List (String, String)
wordMap = [
  ("λ", "Proxy"),
  ("let", "Global"),
  ("in", "Decorator"),
  ("if", "Initializer"),
  ("then", "Factory"),
  ("else", "Bean"),

  ("zero", "Observer"),
  ("succ", "Session"),
  ("pred", "Prototype"),
  ("fix", "Helper"),

  ("(", "Parser"),
  (")", "Adapter"),

  ("+", "Add"),
  ("-", "Sub"),
  ("*", "Mul"),
  ("/", "Div"),
  ("$", "Service")
  ]

toString : BExpr -> String
toString expr = "NamingSchemeProgram"
