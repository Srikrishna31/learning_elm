{-
   All module names must start with a capital letter. A file must contain exactly one module definition in it and the
   filename should match the module name.
   A module definition must contain this keyword because every module must expose at least one value otherwise there's no
   point in creating a module.
   It's important to note that Elm doesn't allow us to export only a subset of data constructors from a type, because they
   are primarily used for pattern matching, and pattern matches need to be exhaustive.

   # Qualified import
   When we import a module without exposing anything inside it, it's called a qualified import. That means we have to
   prefix all functions, types, constants and other values from that module with the module name.

   # Unqualified Import
   When we explicitly expose values while importing a module, it's called an unqualified import, which means we don't neeed
   to prefix the module name.

   # Module names with prefix
   When we import a module from a package that has multiple modules in it, we have to add the package name as a prefix
   except when the module is the same as the package name, for example the Html module. In that case, the package name
   essentially becomes a part of the module name. One exception to this rule is the `Core` package. It contains multiple
   modules, but when we import those modules, we don't have to prefix them with Core. For example, to import the Array
   module all we need to type is import Array and not import Core.Array.
-}


module MyList exposing (..)

{-
   Elm provides three features that are designed specifically for better code organization: modules, packages and the
   Elm Architecture.

   A value is the most basic concept in Elm. 1, a, "Hannibal", and [1,2,3] are all values. An expression allows us to
   compute a value by grouping together other values, operators, and constants. 3*x + 5*y + 10 is an expression. We can
   even use if, case and let to combine multiple expressions and execute them only when certain conditions are met.

   As we write more expressions, we'll inevitably want to reuse some of them in multiple places. We can achieve reusability
   by using functions to encapsulate a bunch of expressions and give names to the collective task those expressions
   accomplish.

   As our program grows, our data structures also tend to become more complex. To be able to easily describe complex
   data structures, we need to first define relationships between different kinds of values used in our program through
   the use of types. As the number of functions and type definitions grow, we need to start grouping together the ones
   that perform similar tasks into modules.

   A module is essentially a collection of functions, constants, type definitions, and other values that can be reused
   in multiple contexts. For example, all functions that perform some kind of operation on a list of values are grouped
   into a module called List. We might even want to combine multiple modules that solve similar problems into a package.
-}


type MyList a
    = Empty
    | Node a (MyList a)


sum : MyList Int -> Int
sum myList =
    case myList of
        Empty ->
            0

        Node intValue reminingNodes ->
            intValue + sum reminingNodes


isEmpty : MyList a -> Bool
isEmpty xs =
    case xs of
        Empty ->
            True

        _ ->
            False


convertToMyList : List a -> MyList a
convertToMyList list =
    let
        listAccumulator : MyList a -> List a -> MyList a
        listAccumulator acc rem =
            case rem of
                [] ->
                    Empty

                v :: rest ->
                    listAccumulator (Node v acc) rest
    in
    listAccumulator Empty list


length : MyList a -> Int
length list =
    case list of
        Empty ->
            0

        Node _ rest ->
            1 + length rest
