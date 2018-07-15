module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Parser exposing (..)
import Compiler exposing (..)
import DeBruijn exposing (..)
import Eval exposing (..)
import Dict exposing (Dict)

main = Html.program {
  init = (initModel, Cmd.none),
  view = view,
  update = update,
  subscriptions = subscriptions
  }

type alias Model = {
  program : String,
  term : Result String BTerm
  }

type Msg =
    EditProgram String
  | RunProgram

initModel = {
  program = "",
  term = Err "No code"
  }

view : Model -> Html Msg
view model =
  div [] [
      div [] [ text "Input PCF program" ],
      div [] [
        textarea [onInput EditProgram] [ text model.program ],
        div [] [ button [onClick RunProgram] [ text "Run" ] ] ],
      case model.term of
        Ok t -> div [] [
                  div [] [ text "DeBruijn Encoded Lambda Caculus." ],
                  div [] [ text (String.join " " (pprBTerm t)) ],
                  div [] [ text "Compiled to Class." ],
                  div [] [ text (compileToClass t) ],
                  div [] [ text "Output" ],
                  div [] [ text (toString (eval t)) ]
                  ]
        Err m -> div [] [ text m ]
    ]

update : Msg -> Model -> (Model, Cmd msg)
update msg model = (updateModel msg model, Cmd.none)

updateModel : Msg -> Model -> Model
updateModel msg model = case msg of
  EditProgram str -> { model | program = str }
  RunProgram -> { model | term = parsePCF model.program
                                 |> Result.andThen (deBruijnEncode [])}

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

