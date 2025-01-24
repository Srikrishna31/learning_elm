module Page.NewPost exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Html exposing (Html, br, button, div, h3, input, text)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick, onInput)
import Http
import Post exposing (Post, PostId, emptyPost, newPostEncoder, postDecoder)
import Route
import Util exposing (buildErrorMessage)


type alias Model =
    { navKey : Nav.Key
    , post : Post
    , createError : Maybe String
    }


type Msg
    = StoreTitle String
    | StoreAuthorName String
    | StoreAuthorURL String
    | CreatePost
    | PostCreated (Result Http.Error Post)


init : Nav.Key -> ( Model, Cmd Msg )
init navKey =
    ( initialModel navKey, Cmd.none )


initialModel : Nav.Key -> Model
initialModel navKey =
    { navKey = navKey
    , post = emptyPost
    , createError = Nothing
    }


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Create New Post" ]
        , newPostForm
        , viewError model.createError
        ]


viewError : Maybe String -> Html Msg
viewError err =
    case err of
        Just error ->
            div []
                [ h3 [] [ text "Couldn't create a post at this time" ]
                , text <| "Error: " ++ error
                ]

        Nothing ->
            text ""


newPostForm : Html Msg
newPostForm =
    Html.form []
        [ div []
            [ text "Title"
            , br [] []
            , input [ type_ "text", onInput StoreTitle ] []
            ]
        , br [] []
        , div []
            [ text "Author Name"
            , br [] []
            , input [ type_ "text", onInput StoreAuthorName ] []
            ]
        , br [] []
        , div []
            [ text "Author URL"
            , br [] []
            , input [ type_ "text", onInput StoreAuthorURL ] []
            ]
        , br [] []
        , div []
            [ button [ type_ "button", onClick CreatePost ]
                [ text "Submit" ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreTitle title ->
            let
                oldPost =
                    model.post

                updateTitle =
                    { oldPost | title = title }
            in
            ( { model | post = updateTitle }, Cmd.none )

        StoreAuthorName name ->
            let
                oldPost =
                    model.post

                updateName =
                    { oldPost | authorName = name }
            in
            ( { model | post = updateName }, Cmd.none )

        StoreAuthorURL url ->
            let
                oldPost =
                    model.post

                updateUrl =
                    { oldPost | authorUrl = url }
            in
            ( { model | post = updateUrl }, Cmd.none )

        CreatePost ->
            ( model, createPost model.post )

        PostCreated (Ok post) ->
            ( { model | post = post }, Route.pushUrl Route.Posts model.navKey )

        PostCreated (Err error) ->
            ( { model | createError = Just <| buildErrorMessage error }, Cmd.none )


createPost : Post -> Cmd Msg
createPost post =
    Http.post
        { url = "http://localhost:5019/posts"
        , body = Http.jsonBody <| newPostEncoder post
        , expect = Http.expectJson PostCreated postDecoder
        }
