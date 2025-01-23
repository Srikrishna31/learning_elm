module Main exposing (main)

--import ListPosts

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, h3, text)
import Page.EditPost as EditPost
import Page.ListPosts as ListPosts
import Route exposing (Route(..))
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Model =
    { route : Route
    , page : Page
    , navKey : Nav.Key
    }


type Page
    = NotFoundPage
    | ListPage ListPosts.Model
    | EditPage EditPost.Model



{-
   Navigation Scenario 1: When the Application Starts
    The Elm runtime takes the full URL entered by the user in the browser's address bar and gives it to the Main.init
    function as below.
    Main.init then extracts a route from that URL by using the Route.parseUrl function and saves it to the main model.
    Based on that route, it also determines which page to display.

    Navigation Scenario 2: When the User clicks a link
    Whenever a link is clicked, the Elm runtime doesn't take the user to that URL directly. It delegates that responsibility
    to our app by sending the message assigned to the onUrlRequest field in main to the Main module's update function.
    Because of this, we now have the ability to save scroll position or persist data or perform some other operation
    before actually taking users to where they want to go.
-}


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        model =
            { route = Route.parseUrl url
            , page = NotFoundPage
            , navKey = navKey
            }
    in
    initCurrentPage ( model, Cmd.none )



{-
   Defining Messages
    Each page in our app will have its own Msg type. To properly manage the interaction between pages, the Main module
    needs to define a separate higher level Msg type of its own. The Main module doesn't handle any page specific messages.
    It simply forwards them to the correct page module.

    UrlRequest is defined in the Browser module like this:
        type UrlRequest
            = Internal Url
            | External String

    Internal Link vs External Link
    As long as a link has the same protocol, host name, and port number as the app, it's considered internal. Otherwise
    it's an external link.

    If the user clicks an internal link, we simply change the URL in a browser's address bar without loading a new page
    or reloading the current one. THe pushUrl function from the Browser.Navigation module does exactly that. This is
    what enables us to create `single-page apps` in Elm. A navigation Key is needed to create navigation commands that
    change the URL in a browser's address bar. This Key is passed on by Elm runtime when calling the init function, which
    should be stored in the model for later uses.

    In contrast, when handling an external link all we need to do is load a new page regardless of what's inside a URL.
    That's why the payload for the External data constructor in UrlRequest is of type String and not Url.

-}


type Msg
    = ListPageMsg ListPosts.Msg
    | LinkClicked UrlRequest
    | UrlChanged Url
    | EditPageMsg EditPost.Msg



{-
   initCurrentPage takes the main model and any commands we may want to fire when the app is being initialized. It then
   looks at the current route and determines which page to initialize. If the route is `NotFound`, we need to display the
   `NotFoundPage` which doesn't need to be initialized. But if the current route is `Posts`, we need to initialize the
   `ListPosts` page by calling its init function.

   Mapping Page Commands

   Although each page is capable of creating its own commands, it can't fire them off to Elm runtime. That responsibility
   lies with the Main module. The page commands are designed to send a page specific message after they are executed. For
   example, the fetchPosts command in ListPosts.elm sends the PostsReceived message which is also defined in ListPosts.elm
   Because Main doesn't know anything about the page specific messages, it needs to map them to one of the data constructors
   from its own Msg type using the Cmd.map function. The Route.Posts branch from initCurrentPage does exactly that.

        Cmd.map : (a -> msg) -> Cmd a -> Cmd msg

    Cmd.map transforms the message produced by a command. (a->msg) is the original message that needs transforming. Remember,
    a message that has a payload is essentially a function. ListPageMsg takes ListPosts.Msg as payload. That's why we are
    able to pass that as the first argument to Cmd.map in the initCurrentPage function.


    After mapping page commands, all that is left is to combine existingCmds with mappedPageCmds. We can do that by using
    the Cmd.batch function. Here's what the Cmd.batch's type signature looks like:

        Cmd.batch : List (Cmd msg) -> Cmd msg

    Cmd.batch takes a list of commands and batches them together so that we can hand them all to the runtime at the same
    time. The runtime then executes them in an arbitrary order.
-}


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                NotFound ->
                    ( NotFoundPage, Cmd.none )

                Posts ->
                    let
                        ( pageModel, pageCmds ) =
                            ListPosts.init
                    in
                    ( ListPage pageModel, Cmd.map ListPageMsg pageCmds )

                Post postId ->
                    let
                        ( pageModel, pageCmd ) =
                            EditPost.init postId model.navKey
                    in
                    ( EditPage pageModel, Cmd.map EditPageMsg pageCmd )
    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )



{-
   # Mapping page commands continued
    When we're dealing with commands, the mapping is handled by Cmd.map. But when we're dealing with HTML, we need to use
    the Html.map function.

        Html.map : (a -> msg) -> Html a -> Html msg
-}


view : Model -> Document Msg
view model =
    { title = "Post App"
    , body = [ currentView model ]
    }


currentView : Model -> Html Msg
currentView model =
    case model.page of
        NotFoundPage ->
            notFoundView

        ListPage pageModel ->
            ListPosts.view pageModel
                |> Html.map ListPageMsg

        EditPage editModel ->
            EditPost.view editModel
                |> Html.map EditPageMsg


notFoundView : Html Msg
notFoundView =
    h3 [] [ text "Oops! The page you requested was not found!" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( ListPageMsg subMsg, ListPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    ListPosts.update subMsg pageModel
            in
            ( { model | page = ListPage updatedPageModel }
            , Cmd.map ListPageMsg updatedCmd
            )

        ( LinkClicked (Browser.Internal url), _ ) ->
            ( model, Nav.pushUrl model.navKey <| Url.toString url )

        ( LinkClicked (Browser.External url), _ ) ->
            ( model, Nav.load url )

        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Route.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none )
                |> initCurrentPage

        ( EditPageMsg subMsg, EditPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    EditPost.update subMsg pageModel
            in
            ( { model | page = EditPage updatedPageModel }
            , Cmd.map EditPageMsg updatedCmd
            )

        ( _, _ ) ->
            ( model, Cmd.none )
