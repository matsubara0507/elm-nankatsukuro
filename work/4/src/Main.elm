module Main exposing (..)

import Html exposing (Html, a, div, span, text, textarea)
import Html.Attributes exposing (class, readonly, type_, value, wrap)
import Html.Events exposing (onClick)
import List.Extra as List


type alias Key =
    { view : String
    , data : List String
    }


keys : List Key
keys =
    [ Key "1 あ" [ "あ", "い", "う", "え", "お", "1" ]
    , Key "2 か" [ "か", "き", "く", "け", "こ", "2" ]
    , Key "3 さ" [ "さ", "し", "す", "せ", "そ", "3" ]
    , Key "4 た" [ "た", "ち", "つ", "て", "と", "4" ]
    , Key "5 な" [ "な", "に", "ぬ", "ね", "の", "5" ]
    , Key "6 は" [ "は", "ひ", "ふ", "へ", "ほ", "6" ]
    , Key "7 ま" [ "ま", "み", "む", "め", "も", "7" ]
    , Key "8 や" [ "や", "ゆ", "よ", "8" ]
    , Key "9 ら" [ "ら", "り", "る", "れ", "ろ", "9" ]
    , Key "*" [ "*" ]
    , Key "0 わ" [ "わ", "を", "ん", "0" ]
    , Key "#" [ "#" ]
    ]


returnButton : Html Msg
returnButton =
    a
        [ class "return button", type_ "button", onClick Return ]
        [ span [ class "icon is-medium" ] [ text "□" ] ]


keyView : Key -> Html Msg
keyView key =
    a
        [ class "button", type_ "button", onClick (Push key) ]
        [ span [ class "icon" ] [ text key.view ] ]



---- MODEL ----


type alias Model =
    { text : String
    , pushed : ( Maybe Key, Int )
    , cache : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" ( Nothing, -1 ) "", Cmd.none )



---- UPDATE ----


type Msg
    = Push Key
    | Return


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Push key ->
            let
                model_ =
                    { model | pushed = updatePushed key model }
            in
            if Tuple.first model.pushed == Just key then
                ( { model_ | cache = updateCache key model_ }, Cmd.none )
            else
                ( { model_ | text = updateText model_, cache = updateCache key model_ }, Cmd.none )

        Return ->
            ( { model | text = updateText model, pushed = ( Nothing, -1 ), cache = "" }, Cmd.none )


updateCache : Key -> Model -> String
updateCache key model =
    key.data
        |> List.getAt (Tuple.second model.pushed)
        |> Maybe.withDefault ""


updateText : Model -> String
updateText model =
    model.text ++ model.cache


updatePushed : Key -> Model -> ( Maybe Key, Int )
updatePushed key model =
    if Tuple.first model.pushed == Just key then
        ( Just key, (Tuple.second model.pushed + 1) % List.length key.data )
    else
        ( Just key, 0 )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [ class "display" ]
            [ textarea
                [ class "display-text"
                , readonly True
                , wrap "hard"
                , value (model.text ++ model.cache)
                ]
                []
            ]
        , keys
            |> List.map keyView
            |> (::) returnButton
            |> div [ class "keyboard" ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
