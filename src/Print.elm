module Print exposing (..)

import String

pExpr : Expr -> String
pExpr expr = case expr of
  Var i -> i
  Abs i body -> "λ" ++ i ++ "." ++ pExpr body
  Bind i value body ->
    String.join " " ["let", i, "=", pExpr value, "in", pExpr body]
  -- TODO use associativity to pretty print and drop some parens
  OpExpr op lhs rhs ->
    parens (pExpr lhs) ++ binOpToString op ++ parens (pExpr rhs)
  App expr1 expr2 -> String.join " " [pAppLeft expr1, pAtom expr2]
  IfZero expr1 expr2 expr3 ->
    String.join " " [
      "if", pExpr expr1,
      "then", pExpr expr2,
      "else", pExpr expr3]
  BVar i -> String.fromInt i
  BAbs body -> "λ" ++ pExpr body
  BBind value body -> String.join " " ["let", pExpr value, "in", pExpr body]
  BPrim i -> i

-- TODO review what is an atom for bin ops
pAtom : Expr -> String
pAtom expr = case expr of
  App _ _ -> parens (pExpr expr)
  _ -> pExpr expr

-- TODO review what is an app left for bin ops
pAppLeft : Expr -> String
pAppLeft expr = case expr of
  Abs _ _ -> parens (pExpr expr)
  BAbs _ -> parens (pExpr expr)
  _ -> pExpr expr

parens : String -> String
parens l = "(" ++ l ++ ")"

binOpToString : BinOp -> String
binOpToString op = case op of
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "/"

