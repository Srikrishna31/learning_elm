module Profile exposing (Name, Name1, createName)

{-
   # Opaque Type
    `Seed` is an opaque type. Opaque types use the same name for type and data constructors to hide internal details.

-}


type alias Name =
    { firstName : String
    , lastName : String
    }


type Name1
    = Name1 String String


createName : String -> String -> Name1
createName firstName lastName =
    Name1 firstName lastName
