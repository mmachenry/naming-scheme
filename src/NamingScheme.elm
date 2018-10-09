module NamingScheme exposing (..)

import List.Extra exposing (dropWhile, takeWhile, find)
import DeBruijn exposing (..)

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
toNamingSchemeWord : ReservedWord -> String
toNamingSchemeWord word = case find (\(l,n)->l==word) wordMap of
  Just (_,n) -> n
  Nothing -> "ERROR"

-- TODO handle error by propogating to parser Result
fromNamingSchemeWord : String -> String
fromNamingSchemeWord word = case find (\(l,n)->n==word) wordMap of
  Just (l,_) -> deBruijnWords l
  Nothing -> "ERROR"

wordMap : List (ReservedWord, String)
wordMap = [
  (RLambda, "Proxy"),
  (RLet, "Global"),
  (RIn, "Decorator"),
  (RIf, "Initializer"),
  (RThen, "Factory"),
  (RElse, "Bean"),

  (RZero, "Observer"),
  (RSucc, "Session"),
  (RPred, "Prototype"),
  (RFix, "Helper"),

  (ROpen, "Parser"),
  (RClose, "Adapter"),

  (RAdd, "Add"),
  (RSub, "Sub"),
  (RMul, "Mul")
  ]

toString : BExpr -> String
toString = buildPrinter toNamingSchemeWord printVarRef ""

printVarRef n = String.repeat n "Meta" ++ "Object"

fromDeBruijnString : String -> String
fromDeBruijnString dbString =
  case lex dbString of
    Ok l -> String.join "" (List.map lexemeToNamingSchemeString l)
    Err str -> str

lexemeToNamingSchemeString : Lexeme -> String
lexemeToNamingSchemeString lexeme = case lexeme of
  LexVar i -> printVarRef i
  LexPrim p -> case p of
                 Fix -> "fix"
                 Zero -> "zero"
                 Succ -> "succ"
                 Pred -> "pred"
  LexRes r -> toNamingSchemeWord r

toDeBruijnString : String -> String
toDeBruijnString nsString = "db"

