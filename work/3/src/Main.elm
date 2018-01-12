module Main exposing (..)

import Html exposing (Html, button, div, h1, input, span, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)
import Process
import Random
import Task
import Time exposing (Time)


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init initModel
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { dieFaces : List Int
    , min : Int
    , max : Int
    , count : Int
    , waitTime : Time
    , number : Int
    }


init : Model -> ( Model, Cmd Msg )
init model =
    ( { model | dieFaces = List.repeat model.number model.min }
    , Cmd.none
    )


initModel : Model
initModel =
    { dieFaces = []
    , min = 1
    , max = 6
    , count = 20
    , waitTime = 30
    , number = 3
    }


type Msg
    = Roll
    | NewFaces (List Int)
    | InputMin Int
    | InputMax Int
    | Wait Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, roll model )

        NewFaces newFaces ->
            if model.count == 0 then
                ( { model | dieFaces = newFaces, count = initModel.count }, Cmd.none )
            else
                update (Wait model.waitTime) { model | dieFaces = newFaces }

        InputMin n ->
            ( { model | min = n }, Cmd.none )

        InputMax n ->
            ( { model | max = n }, Cmd.none )

        Wait time ->
            ( { model | count = model.count - 1 }, wait time )


roll : Model -> Cmd Msg
roll model =
    Random.list model.number (Random.int model.min model.max)
        |> Random.generate NewFaces


wait : Time -> Cmd Msg
wait =
    Process.sleep >> Task.perform (always Roll)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] (List.map viewFace model.dieFaces)
        , div [ class "die-setting" ]
            [ input
                [ class "die"
                , type_ "number"
                , value (toString model.min)
                , onInput (toIntWith initModel.min >> InputMin)
                ]
                []
            , span [] [ text "d" ]
            , input
                [ class "die"
                , type_ "number"
                , value (toString model.max)
                , onInput (toIntWith initModel.max >> InputMax)
                ]
                []
            ]
        , button [ onClick Roll ] [ text "Roll" ]
        ]


viewFace : Int -> Html msg
viewFace n =
    span [ class "face" ] [ text (toString n) ]


toIntWith : Int -> String -> Int
toIntWith default =
    String.toInt >> Result.withDefault default
