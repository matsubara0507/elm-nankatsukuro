module Main exposing (main)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { count : Int
    , kuji : Maybe Kuji
    }


type Msg
    = ClickTsuTsu
    | ClickKuji
    | Shake Int
    | Draw Int


init : ( Model, Cmd Msg )
init =
    ( { count = 0, kuji = Nothing }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ class "omikuji" ]
        [ model.kuji
            |> Maybe.map viewKuji
            |> Maybe.withDefault (div [] [])
        , img
            [ src omikujiUrl
            , height 200
            , style [ ( "transform", "rotate(-16deg)" ) ]
            , style
                (if model.count == 0 then
                    []
                 else if model.count % 2 == 0 then
                    [ ( "animation", "shakeR 1s" ) ]
                 else
                    [ ( "animation", "shakeL 1s" ) ]
                )
            , onClick ClickTsuTsu
            ]
            []
        ]


viewKuji : Kuji -> Html Msg
viewKuji kuji =
    img
        [ src kuji.url
        , class "kuji"
        , onClick ClickKuji
        ]
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickTsuTsu ->
            ( { model | count = model.count + 1 }, shake )

        ClickKuji ->
            init

        Shake n ->
            ( model
            , if n <= model.count then
                draw
              else
                Cmd.none
            )

        Draw n ->
            ( { model | kuji = Array.get n kujis }, Cmd.none )


shake : Cmd Msg
shake =
    Random.generate Draw (Random.int 1 50)


draw : Cmd Msg
draw =
    Random.generate Draw (Random.int 0 (Array.length kujis - 1))


type alias Kuji =
    { name : String
    , url : String
    }


kujis : Array Kuji
kujis =
    [ ( 6, { name = "Kichi", url = "http://2.bp.blogspot.com/-27IG0CNV-ZE/VKYfn_1-ycI/AAAAAAAAqXw/fr6Y72lOP9s/s800/omikuji_kichi.png" } )
    , ( 5, { name = "SyoKichi", url = "http://3.bp.blogspot.com/-nZt5pjGWT9E/T3K7TJ4wEZI/AAAAAAAAE_E/c1X2-N54EYo/s1600/omikuji_syoukichi.png" } )
    , ( 3, { name = "SueKichi", url = "http://3.bp.blogspot.com/-JLNa8mwZRnU/T3K7StR-bEI/AAAAAAAAE-8/rQrDomz5MSw/s1600/omikuji_suekichi.png" } )
    , ( 3, { name = "ChuKichi", url = "http://3.bp.blogspot.com/-_z-n-7gO3KA/T3K7MU3MdGI/AAAAAAAAE-k/8qs-jxqS4LE/s1600/omikuji_chuukichi.png" } )
    , ( 2, { name = "DaiKichi", url = "http://3.bp.blogspot.com/-vQSPQf-ytsc/T3K7QM3qaQI/AAAAAAAAE-s/6SB2q7ltxwg/s1600/omikuji_daikichi.png" } )
    , ( 1, { name = "Kyou", url = "http://4.bp.blogspot.com/-qCfF4H7YOvE/T3K7R5ZjQVI/AAAAAAAAE-4/Hd1u2tzMG3Q/s1600/omikuji_kyou.png" } )
    ]
        |> List.concatMap (uncurry List.repeat)
        |> Array.fromList


omikujiUrl : String
omikujiUrl =
    "http://3.bp.blogspot.com/-cPqdLavQBXA/UZNyKhdm8RI/AAAAAAAASiM/NQy6g-muUK0/s800/syougatsu2_omijikuji2.png"
