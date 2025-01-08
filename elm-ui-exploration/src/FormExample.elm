module FormExample exposing (..)

{-
   Forms with composable-forms package

   hecrj/composable-form is a package that allows us to create forms with input validation and error messages.

   Conceptually, there are two steps: first, defining the data model and then rendering the form. This package cleanly
   separates the visual representation of the form from everything else (data types, validation, field attributes etc.).

   By default, the package allows us to render a form as Html msg value. However, it also includes a facility for defining
   custom renderers, which means that it's possible to render a form as an Element msg value instead.
-}

import Browser
import Colors exposing (blue, darkCharcoal, lightBlue, lightGrey, orange, white)
import Element exposing (Attribute, Element, below, centerY, column, el, fill, htmlAttribute, inFront, layout, mouseOver, moveDown, moveRight, moveUp, none, padding, paddingXY, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (labelAbove, labelRight, placeholder)
import Form exposing (Form)
import Form.Error
import Form.View exposing (Model, State(..))
import Html exposing (Html)
import Html.Attributes


type Msg
    = FormChanged (Form.View.Model ModelData)
    | Signup UserDetails


type alias Model =
    Form.View.Model ModelData


type alias ModelData =
    { agreedToTerms : Bool
    , email : String
    , errors :
        { agreedToTerms : Maybe String
        , email : Maybe String
        , name : Maybe String
        , password : Maybe String
        , plan : Maybe String
        , repeatPassword : Maybe String
        }
    , name : String
    , password : String
    , plan : String
    , repeatPassword : String
    }


type Email
    = Email String


type Password
    = Password String


type Plan
    = Basic
    | Pro
    | Enterprise


type alias UserDetails =
    { name : Maybe String
    , email : Email
    , password : Password
    , plan : Plan
    }


init : Model
init =
    Form.View.idle
        { name = ""
        , email = ""
        , errors =
            { agreedToTerms = Nothing
            , email = Nothing
            , name = Nothing
            , password = Nothing
            , plan = Nothing
            , repeatPassword = Nothing
            }
        , password = ""
        , repeatPassword = ""
        , plan = "Pro"
        , agreedToTerms = False
        }



-- FORM DEFINITION --


nameField : Form ModelData String
nameField =
    Form.textField
        { parser = Ok
        , value = .name
        , update = \value values -> { values | name = value }
        , attributes =
            { label = "Name"
            , placeholder = "Your name"
            }
        , error = .errors >> .name
        }


emailField : Form ModelData Email
emailField =
    Form.emailField
        { parser = parseEmail
        , value = .email
        , update = \value values -> { values | email = value }
        , attributes =
            { label = "Email"
            , placeholder = "you@example.com"
            }
        , error = .errors >> .email
        }


parseEmail : String -> Result String Email
parseEmail s =
    if String.contains "@" s then
        Ok <| Email s

    else
        Err "Invalid Email"


parsePassword : String -> Result String Password
parsePassword s =
    if String.length s >= 6 then
        Ok <| Password s

    else
        Err "Password must be atleast 6 characters"


passwordField : Form ModelData Password
passwordField =
    Form.passwordField
        { parser = parsePassword
        , value = .password
        , update = \value values -> { values | password = value }
        , attributes =
            { label = "Password"
            , placeholder = "Your password"
            }
        , error = .errors >> .password
        }


repeatPasswordField : Form ModelData ()
repeatPasswordField =
    Form.meta
        (\values ->
            Form.passwordField
                { parser =
                    \value ->
                        if value == values.password then
                            Ok ()

                        else
                            Err "The passwords must match"
                , value = .repeatPassword
                , update =
                    \newValue values_ ->
                        { values_ | repeatPassword = newValue }
                , attributes =
                    { label = "Repeat Password"
                    , placeholder = "Repeat Password"
                    }
                , error = .errors >> .repeatPassword
                }
        )


parsePlan : String -> Result String Plan
parsePlan s =
    case s of
        "Basic" ->
            Ok Basic

        "Pro" ->
            Ok Pro

        "Enterprise" ->
            Ok Enterprise

        _ ->
            Err "Invalid Plan"


planSelector : Form ModelData Plan
planSelector =
    Form.selectField
        { parser = parsePlan
        , value = .plan
        , update = \value values -> { values | plan = value }
        , attributes =
            { label = "Choose a plan"
            , placeholder = "Choose a plan"
            , options =
                [ ( "Basic", "Basic" )
                , ( "Pro", "Pro" )
                , ( "Enterprise", "Enterprise" )
                ]
            }
        , error = .errors >> .plan
        }


termsCheckbox : Form ModelData ()
termsCheckbox =
    Form.checkboxField
        { parser =
            \value ->
                if value then
                    Ok ()

                else
                    Err "You must accept the terms"
        , value = .agreedToTerms
        , update = \value values -> { values | agreedToTerms = value }
        , attributes =
            { label = "I agree to terms and conditions"
            }
        , error = .errors >> .agreedToTerms
        }


form : Form ModelData UserDetails
form =
    Form.succeed
        (\name email password plan _ ->
            UserDetails name email password plan
        )
        |> Form.append (Form.optional nameField)
        |> Form.append emailField
        |> Form.append
            (Form.succeed
                (\password _ -> password)
                |> Form.append passwordField
                |> Form.append repeatPasswordField
                |> Form.group
            )
        |> Form.append planSelector
        |> Form.append termsCheckbox



-- CUSTOM FORM RENDERING WITH ELM-UI --


renderElmUiForm :
    Form.View.ViewConfig values Msg
    -> Form values Msg
    -> Form.View.Model values
    -> Element Msg
renderElmUiForm =
    Form.View.custom
        { form = elmUiFormView
        , textField = textFieldView
        , emailField = emailFieldView
        , passwordField = passwordFieldView
        , searchField = searchFieldView
        , textareaField = textareaFieldView
        , numberField = numberFieldView
        , rangeField = rangeFieldView
        , checkboxField = checkboxFieldView
        , radioField = radioFieldView
        , selectField = selectFieldView
        , group = groupView
        , section = sectionView
        , formList = formListView
        , formListItem = formListItemView
        }


textFieldView : Form.View.TextFieldConfig msg -> Element msg
textFieldView { onChange, onBlur, disabled, value, error, showError, attributes } =
    Input.text
        ([] |> withCommonAttrs showError error disabled onBlur)
        { onChange = onChange
        , text = value
        , placeholder = placeholder attributes
        , label = labelAbove (showError && error /= Nothing) attributes
        }


emailFieldView : Form.View.TextFieldConfig msg -> Element msg
emailFieldView { onChange, onBlur, disabled, value, error, showError, attributes } =
    Input.email
        ([] |> withCommonAttrs showError error disabled onBlur)
        { onChange = onChange
        , text = value
        , placeholder = placeholder attributes
        , label = labelAbove (showError && error /= Nothing) attributes
        }


searchFieldView : Form.View.TextFieldConfig msg -> Element msg
searchFieldView { onChange, onBlur, disabled, value, error, showError, attributes } =
    Input.search
        ([] |> withCommonAttrs showError error disabled onBlur)
        { onChange = onChange
        , text = value
        , placeholder = placeholder attributes
        , label = labelAbove (showError && error /= Nothing) attributes
        }


passwordFieldView : Form.View.TextFieldConfig msg -> Element msg
passwordFieldView { onChange, onBlur, disabled, value, error, showError, attributes } =
    Input.currentPassword
        ([] |> withCommonAttrs showError error disabled onBlur)
        { onChange = onChange
        , text = value
        , placeholder = placeholder attributes
        , label = labelAbove (showError && error /= Nothing) attributes
        , show = False
        }


textareaFieldView : Form.View.TextFieldConfig msg -> Element msg
textareaFieldView { onChange, onBlur, disabled, value, error, showError, attributes } =
    Input.multiline
        ([] |> withCommonAttrs showError error disabled onBlur)
        { onChange = onChange
        , text = value
        , placeholder = placeholder attributes
        , label = labelAbove (showError && error /= Nothing) attributes
        , spellcheck = True
        }


numberFieldView : Form.View.NumberFieldConfig msg -> Element msg
numberFieldView { onChange, onBlur, disabled, value, error, showError, attributes } =
    let
        stepAttr =
            attributes.step
                |> Maybe.map String.fromFloat
                |> Maybe.withDefault "any"
    in
    Input.text
        ([]
            |> withHtmlAttribute Html.Attributes.type_ (Just "number")
            |> withHtmlAttribute Html.Attributes.step (Just stepAttr)
            |> withHtmlAttribute (String.fromFloat >> Html.Attributes.max) attributes.max
            |> withHtmlAttribute (String.fromFloat >> Html.Attributes.min) attributes.min
            |> withCommonAttrs showError error disabled onBlur
        )
        { onChange = onChange
        , text = value
        , placeholder = placeholder attributes
        , label = labelAbove (showError && error /= Nothing) attributes
        }


rangeFieldView : Form.View.RangeFieldConfig msg -> Element msg
rangeFieldView { onChange, onBlur, disabled, value, error, showError, attributes } =
    Input.text
        ([]
            |> withHtmlAttribute Html.Attributes.type_ (Just "range")
            |> withHtmlAttribute (String.fromFloat >> Html.Attributes.step) (Just attributes.step)
            |> withHtmlAttribute (String.fromFloat >> Html.Attributes.max) attributes.max
            |> withHtmlAttribute (String.fromFloat >> Html.Attributes.min) attributes.min
            |> withCommonAttrs showError error disabled onBlur
        )
        { onChange = fromString String.toFloat value >> onChange
        , text = value |> Maybe.map String.fromFloat |> Maybe.withDefault ""
        , placeholder = Nothing
        , label = labelAbove (showError && error /= Nothing) attributes
        }


checkboxFieldView : Form.View.CheckboxFieldConfig msg -> Element msg
checkboxFieldView { onChange, onBlur, value, disabled, error, showError, attributes } =
    Input.checkbox
        ([ paddingXY 0 8 ]
            |> withCommonAttrs showError error False onBlur
        )
        { onChange = onChange
        , icon = Input.defaultCheckbox
        , checked = value
        , label =
            labelRight (showError && error /= Nothing) attributes
        }


radioFieldView : Form.View.RadioFieldConfig msg -> Element msg
radioFieldView { onChange, onBlur, disabled, value, error, showError, attributes } =
    Input.radio
        ([ spacing 10, paddingXY 0 8 ]
            |> withCommonAttrs showError error False onBlur
        )
        { onChange = onChange
        , selected = Just value
        , label = labelAbove (showError && error /= Nothing) attributes
        , options =
            List.map
                (\( val, name ) ->
                    Input.option val (text name)
                )
                attributes.options
        }


selectFieldView : Form.View.SelectFieldConfig msg -> Element msg
selectFieldView { onChange, onBlur, disabled, value, error, showError, attributes } =
    -- Use a radio button in lieu of a select
    radioFieldView
        { onChange = onChange
        , onBlur = onBlur
        , disabled = disabled
        , value = value
        , error = error
        , showError = showError
        , attributes =
            { label = attributes.label
            , options = attributes.options
            }
        }


groupView : List (Element msg) -> Element msg
groupView =
    row [ spacing 12 ]


sectionView : String -> List (Element msg) -> Element msg
sectionView title fields =
    column
        [ Border.solid
        , Border.width 1
        , padding 20
        , width fill
        , inFront <|
            el
                [ moveUp 14
                , moveRight 10
                , Background.color darkCharcoal
                , Font.color white
                , padding 6
                , width shrink
                ]
            <|
                text title
        ]
        fields


formListView : Form.View.FormListConfig msg (Element msg) -> Element msg
formListView { forms, add } =
    none


formListItemView : Form.View.FormListItemConfig msg (Element msg) -> Element msg
formListItemView { fields, delete } =
    none


placeholder : { r | placeholder : String } -> Maybe (Input.Placeholder msg)
placeholder attributes =
    Just <|
        Input.placeholder [] <|
            el [ Font.color lightGrey ] <|
                text attributes.placeholder


labelRight : Bool -> { r | label : String } -> Input.Label msg
labelRight showError attributes =
    Input.labelRight ([] |> when showError (Font.color orange)) <|
        el [ centerY ] (text attributes.label)


labelAbove : Bool -> { r | label : String } -> Input.Label msg
labelAbove showError attributes =
    Input.labelAbove ([ paddingXY 0 0 ] |> when showError (Font.color orange)) <|
        text attributes.label


elmUiFormView : Form.View.FormConfig Msg (Element Msg) -> Element Msg
elmUiFormView { onSubmit, action, loading, state, fields } =
    let
        submitButton =
            Input.button
                ([ paddingXY 16 10, Border.rounded 6, Font.bold ]
                    ++ (if onSubmit == Nothing then
                            [ Background.color lightGrey
                            , Font.color darkCharcoal
                            ]

                        else
                            [ Background.color blue
                            , Font.color white
                            , mouseOver
                                [ Background.color lightBlue
                                , Font.color blue
                                ]
                            ]
                       )
                )
                { onPress = onSubmit
                , label =
                    if state == Form.View.Loading then
                        text loading

                    else
                        text action
                }

        formFeedback =
            case state of
                Form.View.Error error ->
                    el [ Font.color orange ] <| text error

                Form.View.Success success ->
                    el [ Font.color darkCharcoal ] <| text success

                _ ->
                    none
    in
    column [ spacing 28, width fill ] <| fields ++ [ formFeedback, submitButton ]



-- HELPERS --


errorToString : Form.Error.Error -> String
errorToString error =
    case error of
        Form.Error.RequiredFieldIsEmpty ->
            "This field is required"

        Form.Error.ValidationFailed validationError ->
            validationError

        Form.Error.External externalError ->
            externalError


fromString : (String -> Maybe a) -> Maybe a -> String -> Maybe a
fromString parse currentValue input =
    if String.isEmpty input then
        Nothing

    else
        parse input
            |> Maybe.map Just
            |> Maybe.withDefault currentValue


withCommonAttrs : Bool -> Maybe Form.Error.Error -> Bool -> Maybe msg -> List (Attribute msg) -> List (Attribute msg)
withCommonAttrs showError error disabled onBlur attrs =
    attrs
        |> when showError
            (below
                (error
                    |> Maybe.map errorToString
                    |> Maybe.map
                        (\errStr ->
                            el [ moveDown 4, Font.color orange, Font.size 14 ] <| text errStr
                        )
                    |> Maybe.withDefault none
                )
            )
        |> whenJust onBlur Events.onLoseFocus
        |> when disabled (Background.color lightGrey)


withHtmlAttribute : (a -> Html.Attribute msg) -> Maybe a -> List (Attribute msg) -> List (Attribute msg)
withHtmlAttribute toAttribute maybeValue attrs =
    Maybe.map (toAttribute >> htmlAttribute >> (\attr -> attr :: attrs)) maybeValue
        |> Maybe.withDefault attrs


whenJust : Maybe a -> (a -> Attribute msg) -> List (Attribute msg) -> List (Attribute msg)
whenJust maybeValue toAttribute attrs =
    Maybe.map (toAttribute >> (\attr -> attr :: attrs)) maybeValue
        |> Maybe.withDefault attrs


when : Bool -> Attribute msg -> List (Attribute msg) -> List (Attribute msg)
when test attr attrs =
    if test then
        attr :: attrs

    else
        attrs



-- WIRING IT ALL TOGETHER --
{-
   To render a form, we need to supply a record with some details: the message produced on input, the labels for the submit
   button, and the validation mode. We also need to supply the form definition (wrapped in a Form.map call to construct an
   appropriate message that carries UserDetails). Finally, we need to provide the form data (model.formData)
-}


view : Model -> Html Msg
view model =
    layout [ padding 20 ] <|
        renderElmUiForm
            { onChange = FormChanged
            , action = "Sign Up"
            , loading = "Signing Up"
            , validation = Form.View.ValidateOnSubmit
            }
            (Form.map Signup form)
            model


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged newModel ->
            newModel

        Signup _ ->
            model


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
