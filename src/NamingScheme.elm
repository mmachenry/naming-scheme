module NamingScheme exposing (..)

namingSchemeWords : ReservedWord -> String
namingSchemeWords r = case r of
  RLambda -> "Proxy"
  RLet -> "Global"
  RIn -> "Decorator"
  RIf -> "Initializer"
  RThen -> "Factory"
  RElse -> "Bean"
  RZero -> "Observer"
  RSucc -> "Session"
  RPred -> "Prototype"
  RFix -> "Helper"
  ROpen -> "Parser"
  RClose -> "Adapter"
  RAppOp -> "Service"

namingSchemeVarRef =
  many (string "Meta") >>= \metaStr->
    string "Object" >>= \_->
      succeed (BVar (List.length metaStr))

