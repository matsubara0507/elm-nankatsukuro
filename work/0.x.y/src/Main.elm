module Main exposing (main)

import Html exposing (Html, a, div, h1, h3, i, p, span, text)
import Html.Attributes exposing (class, href)
import Http
import Json.Decode as Json exposing (Decoder)
import List.Extra as List
import Navigation exposing (Location)
import UrlParser as Url exposing (parseHash, parsePath)


main =
    Navigation.program ChangeUrl
        { init = init model
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { repos : List Repo
    , user : String
    , message : String
    }


model : Model
model =
    Model [] "" ""


type Msg
    = Fetch (Result Http.Error (List Repo))
    | ChangeUrl Location
    | SearchRepos


init : Model -> Location -> ( Model, Cmd Msg )
init model loc =
    update SearchRepos (updateUser loc model)


updateUser : Location -> Model -> Model
updateUser loc moel =
    { model | user = Maybe.withDefault "" (parseHash Url.string loc) }


view : Model -> Html Msg
view model =
    div [ class "text-center p-5" ]
        [ h1 [ class "title" ] [ text "Your Elm力 is ..." ]
        , div [ class "power" ] [ text (toString (count model.repos)) ]
        , div
            [ class "repos", class "boxed-group centered" ]
            [ h3 [] [ text "Your Elm Repositories" ]
            , div [ class "boxed-group-inner" ] (List.map viewRepo model.repos)
            ]
        ]


count : List Repo -> Int
count =
    List.map ((+) 1 << .stars) >> List.sum


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch (Ok result) ->
            ( { model | repos = List.uniqueBy .name (result ++ model.repos) }, Cmd.none )

        Fetch (Err err) ->
            ( { model | message = toString err }, Cmd.none )

        ChangeUrl loc ->
            update SearchRepos (updateUser loc model)

        SearchRepos ->
            ( model, searchRepos model.user )


searchRepos : String -> Cmd Msg
searchRepos name =
    Cmd.batch
        [ searchReposWith ("language:elm+user:" ++ name)
        , searchReposWith ("topic:elm+user:" ++ name)
        ]


searchReposWith : String -> Cmd Msg
searchReposWith query =
    let
        url =
            "https://api.github.com/search/repositories?q=" ++ query

        request =
            Http.get url (Json.field "items" <| Json.list repoDecoder)
    in
    Http.send Fetch request


type alias Repo =
    { name : String
    , url : String
    , stars : Int
    }


repoDecoder : Decoder Repo
repoDecoder =
    Json.map3 Repo
        (Json.field "full_name" Json.string)
        (Json.field "html_url" Json.string)
        (Json.field "stargazers_count" Json.int)


viewRepo : Repo -> Html msg
viewRepo repo =
    div
        [ class "repo mini-repo-list-item" ]
        [ a
            [ class "repo-name", href repo.url ]
            [ p [ class "text-left mb-0" ] [ text repo.name ] ]
        , span
            [ class "stars" ]
            [ text (toString repo.stars), i [ class "fa fa-star" ] [] ]
        ]
