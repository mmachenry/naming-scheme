module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Parser exposing (parsePCF)
import Compiler exposing (pcfToClass)

main = Html.program {
  init = (initModel, Cmd.none),
  view = view,
  update = update,
  subscriptions = subscriptions
  }

type alias Model = {
  pcf : String
  }

type Msg =
  UpdatePCF String

initModel = {
  pcf = ""
  }

view : Model -> Html Msg
view model =
  div [] [
    div [] [
      div [] [ text "Input PCF program" ],
      div [] [ textarea [onInput UpdatePCF] [ text model.pcf ] ] ],
    case parsePCF model.pcf of
      Ok ast ->
        div [] [
          div [] [ text "PCF Abstract syntax tree" ],
          div [] [ text (toString ast) ],
          div [] [ text "Compiled to class result" ],
          div [] [ text (pcfToClass ast) ]
          ]
      Err errorMessage ->
        div [] [
          div [] [ text "PCF Abstract syntax tree" ],
          div [] [ text errorMessage ],
          div [] [ text "Compiled to class result" ],
          div [] [ text "unavailable" ]
          ]
    ]

update : Msg -> Model -> (Model, Cmd msg)
update msg model = (updateModel msg model, Cmd.none)

updateModel : Msg -> Model -> Model
updateModel msg model = case msg of
  UpdatePCF str -> { model | pcf = str }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
