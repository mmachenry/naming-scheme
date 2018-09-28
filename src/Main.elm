module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import String
import Parse exposing (parse)
import Print exposing (pExpr)
import Eval exposing (eval)
import NamingScheme

main = Html.program {
  init = (initModel, Cmd.none),
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
  term : Result String BTerm
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
                     (\p->parse False p |> Result.andThen (DeBruijn.encode []))
                   DeBruijn -> parse True
                   NamingScheme -> NamingScheme.parse
           in currentParser model.program }
  SwitchTo l -> { model | inputLang = l }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div [style []] [
    div [style [("width", "80%"), ("margin","auto")]] [
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
      textarea [style [("width", "100%")],
                rows 20,
                onInput EditProgram]
               [ text model.program ] ]

output : Model -> Html Msg
output model =
    case model.term of
      Ok t -> div [] [
        div [] [ text "De Bruijn Encoded Lambda Caculus." ],
        div [] [ text (toDeBruijnString t) ],
        div [] [ text "Compiled to NamingScheme." ],
        div [] [ text (toNamingSchemeString t) ],
        div [] [
          case eval t of
            Ok result ->
              div [] [
                div [] [ text "De Bruijn output" ],
                div [] [ text (toDeBruijnString result) ],
                div [] [ text "NamingScheme output" ],
                div [] [ text (toNamingSchemeString result) ]
                ]
            Err message -> div [] [ text message ]
          ]
        ]
      Err message -> div [] [ text message ]

radio : String -> (String, msg) -> Html msg
radio group (n, msg) =
  label []
    [ input [ type_ "radio", name group, onClick msg ] [] , text n]

