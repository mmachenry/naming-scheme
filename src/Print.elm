module Print exposing (..)

import Parse exposing (Expr(..))

toDeBruijnString : BExpr -> String
toDeBruijnString = buildPrinter lambdaCalcWords toString " "

toNamingSchemeString : BExpr -> String
toNamingSchemeString =
  buildPrinter namingSchemeWords (\n->String.repeat n "Meta" ++ "Object") ""

buildPrinter :
     (ReservedWord -> String)
  -> (Int -> String)
  -> String
  -> BExpr
  -> String
buildPrinter rword printVarRef seperator =
  let pTerm : BExpr -> String
      pTerm term = case term of
        BVar i -> printVarRef i
        BAbs body -> rword RLambda ++ pTerm body
        BBind value body ->
          String.join
            seperator
            [rword RLet, pTerm value, rword RIn, pTerm body]
        BApp term1 term2 -> pAppLeft term1 ++ seperator ++ pAtom term2
        BIfZero term1 term2 term3 ->
          String.join
            seperator
            ([rword RIf, pTerm term1,
              rword RThen, pTerm term2,
              rword RElse, pTerm term3])
        BPrim prim -> rword prim

      pAtom : BExpr -> String
      pAtom term = case term of
        BApp _ _ -> parens (pTerm term)
        _ -> pTerm term

      pAppLeft : BExpr -> String
      pAppLeft term = case term of
        BAbs _ -> parens (pTerm term)
        _ -> pTerm term

      parens : String -> String
      parens l = rword ROpen ++ l ++ rword RClose

  in pTerm
