module PhotoFolders exposing (Model, Msg, init, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)



{-
   Recursive Custom Types

   Fortunately, whereas type aliases give a name to an existing type, custom types actually define a brand-new type-and
   they can refer to themselves in their own definitions. Custom types that do this are known as a recursive custom types.

   Under the hood, an Elm List is structured like this custom type:

   type MyList elem
       = Empty
       | Prepend elem (MyList elem)

   When we write [1,2,3], it's essentially syntax sugar for Prepend 1 (Prepend 2 (Prepend 3 Empty)).

   Defining Folder as a Recursive custom type
   Notice that, whereas other custom types defined before had multiple variants-for example,
   type Msg = ClickedPhoto String | ClickedSize ThumbnailSize--this custom type has only one variant. It holds plenty of
   information, though, because that one variant contains a record: type Folder = Folder {name:String, ...}.

   This is a common technique in Elm: when a type alias is the wrong fit, you can upgrade it to a custom type with a single
   variant.It's typical when doing this to give the single variant the same name as the type itself-in this case,
   type Folder=Folder { ... }-but we just as easily could have called it something like type Folder = SingleFolder {..}
   instead.

   This upgrade has no runtime cost when you run elm make with the --optimize flag. When Elm's compiler sees a custom type
   with a single variant, it "unboxes" it, such that type Foo = Foo String compiles down to a plain String at runtime.
-}


type Folder
    = Folder
        { name : String
        , photoUrls : List String
        , subfolders : List Folder
        , expanded : Bool
        }



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
    , root : Folder
    }


initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing
    , photos = Dict.empty
    , root = Folder { name = "Loading...", photoUrls = [], subfolders = [], expanded = False }
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Http.get
        { url = "https://elm-in-action.com/folders/list"
        , expect = Http.expectJson GotInitialModel modelDecoder
        }
    )


type Msg
    = ClickedPhoto String
    | GotInitialModel (Result Http.Error Model)
    | ClickedFolder FolderPath


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )

        GotInitialModel (Ok newModel) ->
            ( newModel, Cmd.none )

        GotInitialModel (Err e) ->
            let
                err =
                    Debug.log "Error receiving model" e
            in
            ( model, Cmd.none )

        ClickedFolder folderPath ->
            ( { model | root = toggleExpanded folderPath model.root }, Cmd.none )



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
        [ div [ class "folders" ]
            [ h1 [] [ text "Folders" ]
            , viewFolder End model.root
            ]
        , div [ class "selected-photo" ] [ selectedPhoto ]
        ]


viewFolder : FolderPath -> Folder -> Html Msg
viewFolder path (Folder folder) =
    -- Inline pattern match
    let
        viewSubFolder : Int -> Folder -> Html Msg
        viewSubFolder index subFolder =
            viewFolder (appendIndex index path) subFolder

        folderLabel =
            label [ onClick (ClickedFolder path) ] [ text folder.name ]
    in
    if folder.expanded then
        let
            contents =
                List.append
                    (List.indexedMap viewSubFolder folder.subfolders)
                    (List.map viewPhoto folder.photoUrls)
        in
        div [ class "folder expanded" ]
            [ folderLabel
            , div [ class "contents" ] contents
            ]

    else
        div [ class "folder collapsed" ] [ folderLabel ]


appendIndex : Int -> FolderPath -> FolderPath
appendIndex index path =
    case path of
        End ->
            Subfolder index End

        Subfolder subfolderIndex remainingPath ->
            Subfolder subfolderIndex (appendIndex index remainingPath)


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


viewPhoto : String -> Html Msg
viewPhoto url =
    div [ class "photo", onClick (ClickedPhoto url) ]
        [ text url ]


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


type FolderPath
    = End
    | Subfolder Int FolderPath



{-
      toggleExpanded takes a FolderPath and a Folder, and does one of the following:
        * If FolderPath is End, there are no subfolders to traverse into, so toggle the expanded value on the given folder.
        * If FolderPath is Subfolder targetIndex, look through the given root's subfolders until we find the one at position
   targetIndex. Then call toggleExpanded again, this time passing that subfolder as the new root folder, and passing the
   remaining FolderPath after discarding the Subfolder value we just handled.

   List.indexedMap
    Below are the types for List.map and List.indexedMap:

    List.map       :        (oldVal -> newVal) -> List oldVal -> List newVal
    List.indexedMap: (Int -> oldVal -> newVal) -> List oldVal -> List newVal

    The only thing List.indexedMap does differently from List.map is that it passes an additional value to the transformation
    function: an Int representing the element's index within the list.

    If you call List.indexedMap on the list ["foo", "bar", "baz"], the transformation function will receive 0 and "foo",
    then 1 and "bar", and finally 2 and "baz".
-}


toggleExpanded : FolderPath -> Folder -> Folder
toggleExpanded folderPath (Folder folder) =
    -- Destructuring the Folder custom type inline
    case folderPath of
        End ->
            Folder { folder | expanded = not folder.expanded }

        Subfolder targetIndex remainingPath ->
            let
                subfolders : List Folder
                subfolders =
                    List.indexedMap transform folder.subfolders

                transform : Int -> Folder -> Folder
                transform currentIndex currentSubfolder =
                    if currentIndex == targetIndex then
                        toggleExpanded remainingPath currentSubfolder

                    else
                        currentSubfolder
            in
            Folder { folder | subfolders = subfolders }



{-
   A type alias to represent the Photo info we get from JSON. The JsonPhoto type introduced below is an intermediate
   representation- a value we'll use only to help us translate from one value to another. In particular, a JsonPhoto value
   will help us get from JSON to a Photo record.
-}


type alias JsonPhoto =
    { title : String
    , size : Int
    , relatedUrls : List String
    }


jsonPhotoDecoder : Decoder JsonPhoto
jsonPhotoDecoder =
    Decode.succeed JsonPhoto
        |> required "title" string
        |> required "size" int
        |> required "related_photos" (list string)



{-
   Decode.KeyValuePairs

   We can decode json keys and values using Decode.KeyValuePairs. It has following type:

   keyValuePairs : Decoder val -> Decoder (List (String, val))

   This gives us a decoder that translates JSON objects into key-value tuples. The key's type is always String, because
   JSON object keys are strings by definition. The value's type depends on the Decoder we pass to the keyValuePairs
   function.

   As an example, suppose we called keyValuePairs jsonPhotoDecoder and ran the resulting decoder on our JSON sample from
   earlier:

   {"turtles.jpg": {...}, "beach.jpg": {...}, "maui.jpg":{...}}

   The output would be a list of (String, JsonPhoto) tuples like so:
   output : List (String, JsonPhoto)
   output =
    [ ("2turtles.jpg", {title = "Turtles & sandals", ...})
    , ("beachday.jpg", {title = "At Chang's beach!", ...})
    , ("day1maui.jpg", {title="First day on Maui", ...})
    ]
-}


finishPhoto : ( String, JsonPhoto ) -> ( String, Photo )
finishPhoto ( url, json ) =
    ( url
    , { url = url
      , size = json.size
      , title = json.title
      , relatedUrls = json.relatedUrls
      }
    )


fromPairs : List ( String, JsonPhoto ) -> Dict String Photo
fromPairs pairs =
    pairs
        |> List.map finishPhoto
        |> Dict.fromList


photosDecoder : Decoder (Dict String Photo)
photosDecoder =
    Decode.keyValuePairs jsonPhotoDecoder
        |> Decode.map fromPairs



{-
   Identifying a cyclic definition

   Imagine the following definition:

   myString: String
   myString = List.reverse myString

   Just as a type alias declaration names a type, this myString= declaration names an expression. Anytime the compiler
   encounters myString, it will substitute the expression after the equals sign. This is where things go wrong:

   List.reverse (List.reverse (List.reverse (List.reverse ...

   This expansion never ends, because the compiler substitutes in List.reverse str as soon as it sees myString, then sees
   a myString in that List.reverse myString expression, and therefore substitutes List.reverse myString into that
   expression ... and so on forever.

   Fixing the Cyclic definition by using Decode.Lazy

   We can solve the problem with folderDecoder definition by using Decode.Lazy:

   Decode.lazy: (() -> Decoder val) -> Decoder val

   Once we use the lazy construct, we will no longer have the never-ending expansion problem, because after the compiler
   reaches (\_ -> list folderDecoder), it stops expanding. That expression is already fully formed anonymous function,
   which needs no further expansion.
-}


folderDecoder : Decoder Folder
folderDecoder =
    Decode.succeed folderFromJson
        -- on success, passes decoded name, photos and subfolders to folderFromJson
        |> required "name" string
        |> required "photos" photosDecoder
        |> required "subfolders" (Decode.lazy (\_ -> list folderDecoder))


folderFromJson : String -> Dict String Photo -> List Folder -> Folder
folderFromJson name photos subfolders =
    Folder
        { name = name
        , expanded = True
        , subfolders = subfolders
        , photoUrls = Dict.keys photos
        }


modelPhotosDecoder : Decoder (Dict String Photo)
modelPhotosDecoder =
    Decode.succeed modelPhotosFromJson
        |> required "photos" photosDecoder
        |> required "subfolders" (Decode.lazy (\_ -> list modelPhotosDecoder))



{-
   Dict.union

   union: Dict comparable val -> Dict comparable val -> Dict comparable val

   It iterates over the first dictionary and calls Dict.insert on each of its keys and values inserting them into the second
   dictionary. The returned dictionary has the combined contents of both dictionaries. Because the calls to Dict.insert use
   keys and values from the first dictionary, anytime the second dictionary already happens to have an entry for a particular
   key, it will get overridden.

   List.foldl and List.foldr

   Both List.foldl and List.foldr have the same type:

   (element -> state -> state) -> state -> List element -> state

   * (element -> state -> state) is the update function.
   * state is initial state
   * List element is the list to be folded.

   Fold functions work similar to Elm Architecture's update function:

   Elm Architecture update: Msg -> Model -> Model
   Fold function update: element -> state -> state

   Whenever the Elm Runtime calls update, it passes a Msg and the current Model, and gets back the updates Model it will
   use the next time it calls update. Similarly, each time a fold calls its update function, it passes an element and the
   previous state, and gets back the updated state to use the next time it calls the update function.

   The first time the fold function calls its update function, it passes the initial state as the state argument and the
   first element in the list as the other argument. After repeating this process with the remaining elements in the list,
   it returns the final state value. (If the list is empty, it returns the initial state immediately.)

   Differences between Foldl and Foldr
   Both foldl and foldr begin with the initial state and then call the update function on it four times. But whereas foldl
   passes each element in its list to the update function in the same order as they appear in that list, foldr passes them
   in the reverse order.
-}


modelPhotosFromJson : Dict String Photo -> List (Dict String Photo) -> Dict String Photo
modelPhotosFromJson folderPhotos subfolderPhotos =
    List.foldl Dict.union folderPhotos subfolderPhotos



{-
   Joining Two decoders
   Decode.map2 function transforms the contents of a value - in this case, a Decoder value rather than the entire value.
   The difference is that map2 takes an extra argument:

   map:  (val        -> final) ->                Decoder val -> Decoder final
   map2: (one -> two -> final) -> Decoder one -> Decoder two -> Decoder final

-}


modelDecoder : Decoder Model
modelDecoder =
    Decode.map2
        (\photos root ->
            { photos = photos, root = root, selectedPhotoUrl = Nothing }
        )
        modelPhotosDecoder
        folderDecoder
