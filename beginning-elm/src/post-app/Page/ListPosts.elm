module Page.ListPosts exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Post exposing (Post, PostId, postsDecoder)
import RemoteData exposing (WebData)
import Util exposing (buildErrorMessage)


type alias Model =
    { posts : WebData (List Post)
    }


type Msg
    = FetchPosts
    | PostsReceived (WebData (List Post))


init : ( Model, Cmd Msg )
init =
    ( { posts = RemoteData.Loading }, fetchPosts )


fetchPosts : Cmd Msg
fetchPosts =
    Http.get
        { url = "http://localhost:5019/posts/"
        , expect =
            postsDecoder
                |> Http.expectJson (RemoteData.fromResult >> PostsReceived)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchPosts ->
            ( { model | posts = RemoteData.Loading }, fetchPosts )

        PostsReceived response ->
            ( { model | posts = response }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick FetchPosts ]
            [ text "Refresh posts" ]
        , viewPosts model.posts
        ]


viewPosts : WebData (List Post) -> Html Msg
viewPosts posts =
    case posts of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success actualPosts ->
            div []
                [ h3 [] [ text "Posts" ]
                , table [] <|
                    [ viewTableHeader ]
                        ++ List.map viewPost actualPosts
                ]

        RemoteData.Failure httpError ->
            viewFetchError <| buildErrorMessage httpError


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "ID" ]
        , th []
            [ text "Title" ]
        , th []
            [ text "Author" ]
        ]


viewPost : Post -> Html Msg
viewPost post =
    let
        postPath =
            "/posts/" ++ Post.idToString post.id
    in
    tr []
        [ td []
            [ text <| Post.idToString post.id ]
        , td []
            [ text post.title ]
        , td []
            [ a [ href post.authorUrl ] [ text post.authorName ] ]
        , td []
            [ a [ href postPath ] [ text "Edit" ] ]
        ]


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    div []
        [ h3 [] [ text "Couldn't fetch posts at this time." ]
        , text <| "Error: " ++ errorMessage
        ]
