module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import String
import Parser exposing (..)
import DeBruijn exposing (..)
import Eval exposing (..)
import Dict exposing (Dict)

main = Html.program {
  init = (initModel, Cmd.none),
  view = view,
  update = update,
  subscriptions = subscriptions
  }

type Language = LambdaCalculus | DeBruijn | Class

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
                     (\p->parsePCF p |> Result.andThen (deBruijnEncode []))
                   DeBruijn -> parseDeBruijn
                   Class -> parseClass
           in currentParser model.program }
  SwitchTo l -> { model | inputLang = l }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div [] [
    fieldset [] (List.map (radio "langInput") [
        ("Lambda Calculus", SwitchTo LambdaCalculus),
        ("De Bruijn", SwitchTo DeBruijn),
        ("Class", SwitchTo Class)]),
    div [] [
      textarea [onInput EditProgram] [ text model.program ],
      div [] [ button [onClick RunProgram] [ text "Run" ] ] ],
    case model.term of
      Ok t -> div [] [
        div [] [ text "De Bruijn Encoded Lambda Caculus." ],
        div [] [ text (toDeBruijnString t) ],
        div [] [ text "Compiled to Class." ],
        div [] [ text (toClassString t) ],
        div [] [
          case eval t of
            Ok result ->
              div [] [
                div [] [ text "De Bruijn output" ],
                div [] [ text (toDeBruijnString result) ],
                div [] [ text "Class output" ],
                div [] [ text (toClassString result) ]
                ]
            Err message -> div [] [ text message ]
          ]
        ]
      Err message -> div [] [ text message ]
    ]

radio : String -> (String, msg) -> Html msg
radio group (n, msg) =
  label []
    [ input [ type_ "radio", name group, onClick msg ] [] , text n]
