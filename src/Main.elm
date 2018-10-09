module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import String

import LambdaCalculus
import DeBruijn
import DeBruijnEncode
import NamingScheme
import Eval

main = Browser.element {
  init = (\()->(initModel, Cmd.none)),
  view = view,
  update = update,
  subscriptions = subscriptions
  }

type Language = LambdaCalculus | DeBruijn | NamingScheme

type Msg =
    EditProgram String
  | RunProgram
  | SwitchTo Language

type alias Model = {
  program : String,
  inputLang : Language,
  term : Result String DeBruijn.BExpr
  }

initModel = {
  program = "",
  inputLang = LambdaCalculus,
  term = Err ""
  }

update : Msg -> Model -> (Model, Cmd msg)
update msg model = (updateModel msg model, Cmd.none)

updateModel : Msg -> Model -> Model
updateModel msg model = case msg of
  EditProgram str -> { model | program = str }
  RunProgram -> { model |
    term = let currentParser =
                 case model.inputLang of
                   LambdaCalculus ->
                     (\p->
                          LambdaCalculus.parse p
                       |> Result.andThen (DeBruijnEncode.deBruijnEncode []))
                   DeBruijn -> DeBruijn.parse
                   NamingScheme -> NamingScheme.parse
           in currentParser model.program }
  SwitchTo l -> { model | inputLang = l }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div [] [
    div [style "width" "80%", style "margin" "auto"] [
      div [] [
        fieldset [] (List.map (radio "langInput") [
          ("Lambda Calculus", SwitchTo LambdaCalculus),
          ("De Bruijn", SwitchTo DeBruijn),
          ("NamingScheme", SwitchTo NamingScheme)]),
        editor model,
        output model
        ]
    ]
 ]

editor : Model -> Html Msg
editor model =
  div [] [
      div [] [ button [onClick RunProgram] [ text "Run" ] ],
      textarea [style "width" "100%",
                rows 8,
                onInput EditProgram]
               [ text model.program ] ]

output : Model -> Html Msg
output model =
  div [] [
    h1 [] [ text"Direct lexical substitution" ],
    div [] [ text <|
      case model.inputLang of
        LambdaCalculus -> "not relavent"
        DeBruijn -> NamingScheme.fromDeBruijnString model.program
        NamingScheme -> NamingScheme.toDeBruijnString model.program
    ],
    case model.term of
      Ok t ->
        case Eval.eval t of
          Ok result -> displayResults t result
          Err message -> div [] [ text message ]
      Err message -> div [] [ text message ]
  ]

displayResults : DeBruijn.BExpr -> DeBruijn.BExpr -> Html Msg
displayResults expr result = div [] [
  h1 [] [ text "DeBruijn" ],
  h2 [] [ text "Expression" ],
  div [] [ text (DeBruijn.toString expr) ],
  h2 [] [ text "Result" ],
  div [] [ text (DeBruijn.toString result) ],
  h1 [] [ text "NamingScheme" ],
  h2 [] [ text "Expression" ],
  div [] [ text (NamingScheme.toString expr) ],
  h2 [] [ text "Result" ],
  div [] [ text (NamingScheme.toString result) ],
  h1 [] [ text "Debug" ],
  h2 [] [ text "AST" ],
  div [] [ text (Debug.toString expr) ],
  h2 [] [ text "Numeric value" ],
  div [] [ text (Debug.toString (Eval.exprToNum result)) ]
  ]

radio : String -> (String, msg) -> Html msg
radio group (n, msg) =
  label []
    [ input [ type_ "radio", name group, onClick msg ] [] , text n]

