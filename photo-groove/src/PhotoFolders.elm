module PhotoFolders exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)



{-
      Constrained Type variables: comparable, number, appendable
   The type annotation for Dict.get is (comparable -> Dict comparable value -> Maybe value). The comparable is one of the
   exceptional type variables. Following are the reserved type variables in Elm:

   * number, which can resolve to Int or Float
   * appendable, which can resolve to String or List
   * comparable, which can resolve to Int, Float, Char, String, List or a tuple of these.

    For example, the type of multiplication operator is (*): number -> number -> number.
    So, number can resolve to one of the following:
    Int -> Int -> Int
    Float -> Float -> Float

    The number -> number -> number function will resolve to one of these after it gets passed either an Int or Float as
    one of its arguments. This means that multiplying two Ints will return an Int, multiplying two Floats will return a
    Float, and attempting to multiply an Int by a Float will result in type mismatch.

    If you need to annotate a function that takes two independent number types, you can add more characters after the word
    number to create distinct type variable names that are still constrained by number. For example, number, numberB,
    number2 etc.

    The number constraint typically appears in core functions having to do with mathematics. appendable is used by the
    (++) operator. comparable appears in the definitions of Dict and Set, as well as in List.sort and mathematical inequality
    operators like (<) and (>=). Data structures like Dict and Set use comparable constraint to store their contents
    efficientl, which involves sorting them - and only the comparable types have implicit sorting functions built in.

    In cases of Dict, its keys must be comparable, but its values can be any type.
-}


type alias Model =
    { selectedPhotoUrl : Maybe String
    , photos : Dict String Photo
    }


initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing
    , photos = Dict.empty
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Http.get
        { url = "https://elm-in-action.com/folders/list"
        , expect = Http.expectJson GotInitialModel modelDecoder
        }
    )


modelDecoder : Decoder Model
modelDecoder =
    Decode.succeed
        { selectedPhotoUrl = Just "trevi"
        , photos =
            Dict.fromList
                [ ( "trevi"
                  , { title = "Trevi"
                    , relatedUrls = [ "coli", "fresco" ]
                    , size = 34
                    , url = "trevi"
                    }
                  )
                , ( "fresco"
                  , { title = "Fresco"
                    , relatedUrls = [ "trevi" ]
                    , size = 46
                    , url = "fresco"
                    }
                  )
                , ( "coli"
                  , { title = "Coliseum"
                    , relatedUrls = [ "trevi", "fresco" ]
                    , size = 35
                    , url = "coli"
                    }
                  )
                ]
        }


type Msg
    = ClickedPhoto String
    | GotInitialModel (Result Http.Error Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )

        GotInitialModel (Ok newModel) ->
            ( newModel, Cmd.none )

        GotInitialModel (Err _) ->
            ( model, Cmd.none )



{-
   Comparing andThen and map
   Result.andThen : (a -> Result x b) -> Result x a -> Result x b
   Result.map     : (a ->          b) -> Result x a -> Result x b

   Both functions accept a callback and a Result; both return the given value unchanged when given an Err variant; and
   when given an Ok, both pass the value inside that Ok to their callbacks.

   They differ in that if Result.map receives an Ok variant, it always returns an Ok variant. The content of that Ok
   variant might be different, but it will definitely be an Ok and not an Err. In contrast, Result.andThen returns whatever
   its callback returns- which means it can potentially receive an Ok and turn it into an Err.

   The Maybe.map function has a very similar relationship to Maybe.andThen:

   Maybe.andThen : (a -> Maybe b) -> Maybe a -> Maybe b
   Maybe.map     : (a ->       b) -> Maybe a -> Maybe b

   Similarly to the behavior of Result.map, if Maybe.map receives a Just, it always returns a Just. If it receives Nothing,
   it returns Nothing.

   Similarly to the behavior of Result.andThen, Maybe.andThen returns whatever its callback returns-meaning it can potentially
   receive a Just and turn it into a Nothing.

   This additional capability makes andThen strictly more powerful than map. Anything we can implement with map, we could
   implement with andThen instead. However, map is often preferred in practice because it's more concise, and more often
   than not, the extra power of andThen is not needed.
-}


view : Model -> Html Msg
view model =
    let
        photoByUrl : String -> Maybe Photo
        photoByUrl url =
            Dict.get url model.photos

        selectedPhoto : Html Msg
        selectedPhoto =
            case Maybe.andThen photoByUrl model.selectedPhotoUrl of
                Just photo ->
                    viewSelectedPhoto photo

                Nothing ->
                    text ""
    in
    div [ class "content" ]
        [ div [ class "selected-photo" ] [ selectedPhoto ] ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Photo =
    { title : String
    , size : Int
    , relatedUrls : List String
    , url : String
    }


viewSelectedPhoto : Photo -> Html Msg
viewSelectedPhoto photo =
    div
        [ class "selected-photo" ]
        [ h2 [] [ text photo.title ]
        , img [ src (urlPrefix ++ "photos/" ++ photo.url ++ "/full") ] []
        , span [] [ text (String.fromInt photo.size ++ "KB") ]
        , h3 [] [ text "Related" ]
        , div [ class "related-photos" ]
            (List.map viewRelatedPhoto photo.relatedUrls)
        ]


viewRelatedPhoto : String -> Html Msg
viewRelatedPhoto url =
    img
        [ class "related-photo"
        , onClick (ClickedPhoto url)
        , src (urlPrefix ++ "photos/" ++ url ++ "/thumb")
        ]
        []


urlPrefix : String
urlPrefix =
    "https://elm-in-action.com/"
