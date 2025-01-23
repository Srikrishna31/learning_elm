module Page.EditPost exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Html exposing (Html, br, button, div, h3, input, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Post exposing (Post, PostId, postDecoder, postEncoder)
import RemoteData exposing (WebData)
import Route
import Util exposing (buildErrorMessage)


type Msg
    = PostReceived (WebData Post)
    | UpdateTitle String
    | UpdateAuthorName String
    | UpdateAuthorUrl String
    | SavePost
    | PostSaved (Result Http.Error Post)


type alias Model =
    { navKey : Nav.Key
    , post : WebData Post
    , saveError : Maybe String
    }


init : PostId -> Nav.Key -> ( Model, Cmd Msg )
init postId navKey =
    ( initialModel navKey, fetchPost postId )


initialModel : Nav.Key -> Model
initialModel navKey =
    { navKey = navKey
    , post = RemoteData.Loading
    , saveError = Nothing
    }



{-
   # Fetching Post
   When the `EditPost` page is being initialized, we need to fetch a fresh copy of the post we want to edit from the
   server. The `Main` module could have grabbed the post record in question from the `ListPosts` page and sent that
   directly to `EditPost` instead of just the ID. That would have saved us a round trip to the server, but if another
   client app has already modified the post, it'd be a problem. By fetching it from the server, we're always looking on
   the latest version of that record.
-}


fetchPost : PostId -> Cmd Msg
fetchPost postId =
    Http.get
        { url = "http://localhost:5019/posts/" ++ Post.idToString postId
        , expect =
            postDecoder
                |> Http.expectJson (RemoteData.fromResult >> PostReceived)
        }



{-
   Here's how RemoteData.map is implemented behind the scenes:

       map: (a -> b) -> RemoteData e a -> RemoteData e b
       map f data =
           case data of
               Success value ->
                   Success <| f value
               Loading ->
                   Loading
               NotAsked ->
                   NotAsked
               Failure error ->
                   Failure error
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PostReceived post ->
            ( { model | post = post }, Cmd.none )

        UpdateTitle newTitle ->
            let
                updateTitle =
                    RemoteData.map
                        (\postData ->
                            { postData | title = newTitle }
                        )
                        model.post
            in
            ( { model | post = updateTitle }, Cmd.none )

        UpdateAuthorName newName ->
            let
                updateAuthorName =
                    RemoteData.map
                        (\postData ->
                            { postData | authorName = newName }
                        )
                        model.post
            in
            ( { model | post = updateAuthorName }, Cmd.none )

        UpdateAuthorUrl newUrl ->
            let
                updateAuthorUrl =
                    RemoteData.map
                        (\postData ->
                            { postData | authorUrl = newUrl }
                        )
                        model.post
            in
            ( { model | post = updateAuthorUrl }, Cmd.none )

        SavePost ->
            ( model, savePost model.post )

        PostSaved (Ok postData) ->
            let
                post =
                    RemoteData.succeed postData
            in
            ( { model | post = post, saveError = Nothing }
            , Route.pushUrl Route.Posts model.navKey
            )

        PostSaved (Err error) ->
            ( { model | saveError = Just (buildErrorMessage error) }, Cmd.none )



{-
   Http.request takes a recode with seven fields:

   method - To update a resource on server, we need to use the `PATCH` method.

   headers - The `headers` field allows us to send additional information to the server.

   url - The location of the resource we want to modify.

   body - This field contains the modified post data. We can't assign the encoded value directly to the body field though.
   We need to explicitly tell Http.request that our encoded value is in JSON format by using the Http.jsonBody function.
   This will add the `Content-Type: application/json` header to our HTTP request behind the scenes. That is how the server
   knows the body of a request is in JSON format.

   expect - By using the `Http.expectJson` function we're letting Elm know that we expect the response body to be JSON
   as well.

   timeout - Sometimes a server takes forever to return a response. If we don't want our users to wait tool long, we can
   specify a timeout.

   tracker - This field allows us to track the progress of a request.

   When the `PATCH` request is complete, the Elm runtime will send the `PostSaved` message to `update`.
-}


savePost : WebData Post -> Cmd Msg
savePost post =
    case post of
        RemoteData.Success postData ->
            let
                postUrl =
                    "http://localhost:5019/posts/"
                        ++ Post.idToString postData.id
            in
            Http.request
                { method = "PATCH"
                , headers = []
                , url = postUrl
                , body = Http.jsonBody <| postEncoder postData
                , expect = Http.expectJson PostSaved postDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

        _ ->
            Cmd.none


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Edit Post" ]
        , viewPost model.post
        , viewSaveError model.saveError
        ]


viewPost : WebData Post -> Html Msg
viewPost post =
    case post of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading Post..." ]

        RemoteData.Success postData ->
            editForm postData

        RemoteData.Failure httpError ->
            viewFetchError <| buildErrorMessage httpError


viewSaveError : Maybe String -> Html msg
viewSaveError maybeError =
    case maybeError of
        Just error ->
            div []
                [ h3 [] [ text "Couldn't save post at this time." ]
                , text <| "Error: " ++ error
                ]

        Nothing ->
            text ""


editForm : Post -> Html Msg
editForm post =
    Html.form []
        [ div []
            [ text "Title"
            , br [] []
            , input
                [ type_ "text"
                , value post.title
                , onInput UpdateTitle
                ]
                []
            ]
        , br [] []
        , div []
            [ text "Author Name"
            , br [] []
            , input
                [ type_ "text"
                , value post.authorName
                , onInput UpdateAuthorName
                ]
                []
            ]
        , br [] []
        , div []
            [ text "Author URL"
            , br [] []
            , input
                [ type_ "text"
                , value post.authorUrl
                , onInput UpdateAuthorUrl
                ]
                []
            ]
        , br [] []
        , div []
            [ button [ type_ "button", onClick SavePost ]
                [ text "Submit" ]
            ]
        ]


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch post at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text <| "Error: " ++ errorMessage
        ]
