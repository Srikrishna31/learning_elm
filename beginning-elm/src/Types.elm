module Types exposing (..)

{-
   In Elm, the first operation applied to the arguments determines the type of the arguments to the function.
   Similarly, the last operation in a function determines the return type of the function.
   Following are the benefits of type annotations:
    * Provides documentation
    * Enables code validation
    * Limits types of values a function can accept.

   An `appendable` is a type variable that can represent a list, string, or text-those are the only types in Elm that can
   be appended together using ++.


   # Currying
    The process of evaluating a function that takes multiple arguments by converting it into a sequence of functions each
    taking a single argument is known as currying.


    # Creating our own types
    Elm allows us to create custom types to address the needs to describe and structure complex data processed by applications.
    Custom types are created by specifying type (and omitting the keyword alias).

    For example, the Greeting type below defines a type that has two values: Howdy and Hola.

   A custom type is often used with a case expression to pattern match a value of that type. Once a match is found, the
   corresponding expression is evaluated. There's nothing special about how we use a custom type. The Bool type provided
   by Elm can also be used in a similar fashion

   # Custom types with Payloads

      Elm makes it easier to describe complex data structures by letting us add a payload to each value in a custom type.
      Unlike Howdy and Hola, Namaste and NumericalHi aren't values by themselves. Instead they provide a way to create
      value (or data). That's why they are called data constructors.
      Interestingly enough, data constructors are actually functions behind the scenes. They take a payload as an argument
      and create values of type Greeting(or any other data constructor). There's no limit to how many and what type of data
      a payload can contain.

      It's important to keep in mind that the data constructors don't behave like normal functions in terms of performing
      an operation on data. They're more like boxes to put data into. They don't do anything with that data other than
      carry them around.

      Since Howdy and Hola take no payload, their values don't need to be constructed. Their values are already established
      which essentially makes them constants. Although they behave like constants, they are also called as nullary data
      constructors. A nullary data constructor is a constructor that takes no arguments.


   # Union Types
    All custom types created using the type keyword are called union types. They are sometimes referred to as tagged unions
    or algebraic data types (ADTs).
    If two sets don't have any elements in common, they are called disjoint sets and their union is called disjoint union.
    In a disjoint union, it's always possible to tell where each element came from.

    If two sets do have some elements in common, it's still possible to create a disjoint union by tagging the elements
    in each set.
            X = {1, 2, 3}
            Y = {3, 4, 5}

     Tagging is the process of creating a  pair of elements, where the first value is the name of the set, and the second
     value is the element itself. This way, any two sets having common elements, can be converted into disjoint sets.
            X* = {(X, 1), (X, 2), (X, 3)} -
            Y* = {(Y, 3), (Y, 4), (Y, 5)}

       X |_| Y = X* U Y* = {(X,1), (X,2), (X,3), (Y, 3), (Y, 4), (Y,5)}


     A union type in Elm is similar to a disjoint union set of tagged elements. For example, the Greeting type can be
     thought of as a disjoint union of four sets:
        * A set that contains Howdy as its only element.
        * A set that contains Hola as its only element.
        * A set that contains infinite number of strings each tagged with Namaste.
        * A set that contains infinite number of two Int values tagged with NumericalHi.




-}

import Regex exposing (Regex)


type
    Greeting
    -- Disjoint Union
    = Howdy -- Empty Tags
    | Hola -- Empty Tags
    | Namaste String -- Tags with values
    | NumericalHi Int Int -- Tags with values


sayHello : Greeting -> String
sayHello greeting =
    case greeting of
        Howdy ->
            "How y'all doin ?"

        Hola ->
            "Hola amigo!"

        Namaste message ->
            message

        NumericalHi value1 value2 ->
            value1 + value2 |> String.fromInt



{-

-}


welcomeMessage : Bool -> String
welcomeMessage isLoggedIn =
    case isLoggedIn of
        True ->
            "Welcome to my awesome site!"

        False ->
            "Please log in."



{-
   It's important to note that type and type alias are two different concepts. type creates a new type, whereas type alias
   gives a new name to an existing type.
   Type aliases make it easier for us to write succinct code.
   As our application grows, our data structures also tend to get more complex. By giving names to these complex data
   structures, we can write code that's much more readable without losing all the benefits we get from the type system.
-}


type alias UrlDup =
    { protocol : Protocol
    , host : String
    , port_ : Maybe String
    , path : String
    , query : Maybe String
    , fragment : Maybe String
    }


type Protocol
    = Tcp
    | Udp
    | Ftp
    | Http
    | Https



{-
   # Type Constructor

   Maybe is a built-in type in Elm that allows to express the idea of a missing value. Maybe by itself is not a valid type.
   It merely provides a way for us to construct a type. That's why it's called a type constructor. It must be applied to
   another type argument for it to generate a valid type. Maybe Int, Maybe String, Maybe (List Number) are all valid
   types.

   Generic (or parameterized) types such as Maybe a can be incredibly powerful. To create our own generic types all we have
   to do is pass an argument to a type constructor.

   A type argument that doesn't get used in any of the data constructors is known as a `phantom type argument`.
-}


type Mabye_ a
    = Just_ a
    | Nothing_



{-
   # Multiple Type Arguments
   Elm allows us to have multiple arguments in a type definition.
-}


type Result_ error value
    = Ok_ value
    | Err_ error


signUp : String -> String -> Result String String
signUp email ageStr =
    case String.toInt ageStr of
        Nothing ->
            Err "Age must be an integer."

        Just age ->
            let
                emailPattern : String
                emailPattern =
                    "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b"

                regex : Regex
                regex =
                    Regex.fromString emailPattern
                        |> Maybe.withDefault Regex.never

                isValidEmail =
                    Regex.contains regex email
            in
            if age < 13 then
                Err "You need to be at least 13 years old to sign up."

            else if isValidEmail then
                Ok "Your account has been created successfully!"

            else
                Err "You entered an invalid email"



{-
   # Type vs Data Constructor

   Type constructors are mainly used either in a type-declaration or a type annotation, whereas data constructors are
   used inside a function body or when we define a top-level constant.
-}


type alias Character =
    { name : String
    , age : Maybe Int
    }


sansa : Character
sansa =
    { name = "Sana"
    , age = Just 19
    }


arya : Character
arya =
    { name = "Arya"
    , age = Nothing
    }


getAdultAge :
    Character
    -> Maybe Int -- Maybe Int is the type constructor
getAdultAge character =
    case character.age of
        Nothing ->
            Nothing

        Just age ->
            if age >= 18 then
                Just age

            else
                Nothing



{-
   # Recursive Types
   Let's look at the type definition below. What it means is that a list of type MyList can be either Empty or Noe a
   followed by another list (MyList a). A list with no elements can be represented like this: Empty. A list with a
   single element is represented like this: Node a Empty. Similarly, a list with two elements is represented like this:
   (Node a (Node a Empty)) and so on.

   It's important to note that if a recursive type doesn't provide at least one nullary data constructor, then we end up
   with a value that never ends.

   # Working with Recursive Types

   We can use recursive types the same way we use any other union type. A case expression is used to pattern match each
   individual data constructor defined in the type.

   Recursive types are very powerful. They enable us to define
-}


type MyList a
    = Empty
    | Node a (MyList a)


sum : MyList Int -> Int
sum myList =
    case myList of
        Empty ->
            0

        Node intValue remainingNodes ->
            intValue + sum remainingNodes


type Tree a
    = TreeEmpty
    | TreeNode a (Tree a) (Tree a)


exampleTree : Tree Char
exampleTree =
    TreeNode '1'
        (TreeNode '2'
            (TreeNode '4'
                TreeEmpty
                (TreeNode '8' TreeEmpty TreeEmpty)
            )
            (TreeNode '5' TreeEmpty TreeEmpty)
        )
        (TreeNode '3'
            (TreeNode '6' TreeEmpty TreeEmpty)
            (TreeNode '7'
                (TreeNode '9' TreeEmpty TreeEmpty)
                TreeEmpty
            )
        )
