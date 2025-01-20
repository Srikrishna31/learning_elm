module DecodingJson exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
    exposing
        ( Decoder
        , int
        , list
        , string
        )
import Json.Decode.Pipeline exposing (required)
import RemoteData exposing (RemoteData, WebData)


type alias Author =
    { name : String
    , url : String
    }


type alias Post =
    { id : Int -- Json-server is converting integers to string, so keep this id as string for now.
    , title : String
    , authorName : String
    , authorUrl : String
    }



{-
   type RemoteData error value
       = NotAsked
       | Loading
       | Failure error
       | Success value

   WebData represents data fetched from an HTTP (also known as Web) server.

   type alias WebData a =
         RemoteData Http.Error a
-}


type alias Model =
    { posts : WebData (List Post)
    }


view : Model -> Html Msg
view model =
    div []
        [ button
            [ onClick SendHttpRequest ]
            [ text "Get data from server" ]
        , viewPostsOrError model
        ]


viewPostsOrError : Model -> Html Msg
viewPostsOrError model =
    case model.posts of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h1 [] [ text "Loading..." ]

        RemoteData.Failure httpError ->
            viewError <| buildErrorMessage httpError

        RemoteData.Success posts ->
            viewPosts posts


viewError : String -> Html Msg
viewError errorMessage =
    div []
        [ h3 [] [ text "Couldn't fetch data at this time" ]
        , text <| "Error" ++ errorMessage
        ]


viewPosts : List Post -> Html Msg
viewPosts posts =
    div []
        [ h3 [] [ text "Posts" ]
        , table []
            ([ viewTableHeader ] ++ List.map viewPost posts)
        ]


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
    tr []
        [ td [] [ text <| String.fromInt post.id ]
        , td [] [ text post.title ]
        , td []
            [ a [ href post.authorUrl ] [ text post.authorName ] ]
        ]


type Msg
    = SendHttpRequest
    | DataReceived (WebData (List Post))


authorDecoder : Decoder Author
authorDecoder =
    Decode.succeed Author
        |> required "name" string
        |> required "url" string



{-
   The `Decode.succeed` function ignores the given JSON and always produces a specific value.

   # Decoding nested fields with requiredAt
   `requiredAt` takes a list of field names and traverses them in order. Once it reaches the last field, it applies the
   given decoder to it.

   # Decoding Nested fields with optionalAt
-}


postDecoder : Decoder Post
postDecoder =
    --map3 Post
    --    (field "id" int)
    --    (field "title" string)
    --    (field "author" string)
    Decode.succeed Post
        |> required "id" int
        |> required "title" string
        |> required "authorName" string
        |> required "authorUrl" string


httpCommand : Cmd Msg
httpCommand =
    Http.get
        { url = "http://localhost:5019/posts"
        , expect = Http.expectJson (RemoteData.fromResult >> DataReceived) <| list postDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( { model | posts = RemoteData.Loading }, httpCommand )

        DataReceived response ->
            ( { model | posts = response }, Cmd.none )


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server"

        Http.BadStatus int ->
            "Request failed with status code: " ++ String.fromInt int

        Http.BadBody message ->
            message


init : () -> ( Model, Cmd Msg )
init _ =
    ( { posts = RemoteData.NotAsked }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
