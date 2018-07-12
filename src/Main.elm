module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String

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
    textarea [onInput UpdatePCF] [ text model.pcf ],
    div [] [ text (pcfToClass model.pcf) ] ]

pcfToClass : String -> String
pcfToClass str = String.toUpper str

update : Msg -> Model -> (Model, Cmd msg)
update msg model = (updateModel msg model, Cmd.none)

updateModel : Msg -> Model -> Model
updateModel msg model = case msg of
  UpdatePCF str -> { model | pcf = str }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
