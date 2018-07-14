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
  pcf : String,
  term : Result String Term
  }

type Msg =
    EditProgram String
  | RunProgram

initModel = {
  pcf = "",
  term = Err "No code"
  }

view : Model -> Html Msg
view model =
  div [] [
      div [] [ text "Input PCF program" ],
      div [] [
        textarea [onInput EditProgram] [ text model.pcf ],
        button [onClick RunProgram] [ text "Run" ] ],
      case model.term of
        Ok t -> div [] [
                  div [] [ text "Program" ],
                  div [] [ text (pprTerm t) ],
                  div [] [ text "AST" ],
                  div [] [ text (toString model.term) ],
                  div [] [ text "Output" ],
                  div [] [ text (toString (eval t)) ]
                  ]
        Err m -> div [] [ text m ]
    ]

update : Msg -> Model -> (Model, Cmd msg)
update msg model = (updateModel msg model, Cmd.none)

updateModel : Msg -> Model -> Model
updateModel msg model = case msg of
  EditProgram str -> { model | pcf = str }
  RunProgram -> { model | term = parsePCF model.pcf }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
