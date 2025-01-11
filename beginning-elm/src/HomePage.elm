module HomePage exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Regex exposing (Regex)



{-
   Regular Expression Basics

       A regular expression (regex) is a pattern for matching character combinations in a string.

    # Matching a single character
       In a regular expression, letters and numbers match themselves. For example, A wil match A and 1 will match 1. All
       regular expressions are case-sensitive, so A won't match a.

    # Matching multiple characters
       To match multiple characters, we just need to repeat the character. AAA will match three A s in a row and 123 will
       match the number 123.

     # Dot character
       Instead of repeating a character, we can use the dot character to match multiple characters like this: A.. . The
       dot character matches a single character. A character can be a letter, number, or any special character. So A.. will
       match AAA, ABB, ACC, ABC and so on. It will also match A12, A$3, A^% etc. Punctuation characters such as . allow
       us to create patterns instead of explicitly specifying all characters in a substring.

       The punctuation characters have special meanings in a regular expression. So if we want to match a literal dot, we
       must precede it with a backslash which turns off dot's special meaning allowing regex to treat it as a regular
       character. Here are some examples that match a literal dot:

            * \. will match a literal dot.
            * Dr\. Strange will match Dr. Strange
            * 7\.67 will match 7.67

    # Matching a set
      Sets allow us to match one character from a set of characters.
            * [0123456789] will match a single digit
            * [aeiou] will match a single lowercase vowel.

    # Using ranges to make sets more succinct
      Sets don't scale well. For example, if we want to match two lowercase letters, we will have to use this:
      [abcdefghijklmnopqrstuvwxyz][abcdefghijklmnopqrstuvwxyz]. This is where a range comes handy. We can rewrite that
      super long regex with ranges like this: [a-z][a-z]. All we need to do is specify the beginning and end of a sequence
      of characters separated by a dash.
      Since certain character sets are used often, regex provides a series of shorthands for representing them. Here are
      some examples:
            * \d will match any digit. It is short for [0-9]
            * \w will match any word character. It is short for [A-Za-z0-9_].
            * \s will match any whitespace character. It is short for [\t\r\n\f]
            * \s\d will match a whitespace character followed by a digit.
            * [\da-f] will match a hexadecimal digit. It is short for [0-9a-f]. Here we combined a shorthand with a range.

    # Matching alternatives
        Sometimes we need to match this or that character. We can use a vertical bar for that.
            * X|Y will match either X or Y.
            * am|a\.m\.|pm|p\.m\. will match am or a.m., or pm or p.m.
            * [0-9]|[a-zA-Z] will match a digit or a letter.

    # Matching Zero or mare characters with asterisk
        Asterisk (*) is probably the most powerful character in a regular expression. Like the dot character, it has a
        special meaning. It matches zero or more of the thing that came just before it.
            * a* will match a. It will also match aa or aaa or aaaa and so on.
            * ab* will match a, ab, abb, abbb, abbb etc.
            * a*b will match b, ab, aaab, aaaaab, etc.
            * [0-9]* will match any number of digits.
            * [a-z]* will match any number of lowercase letters
            * [a-zA-Z0-9_]* will match any number of word characters (letter, number or underscore).
            * \d* will match any number of digits.
            * \w* will match any number of word characters.
            * \s* will match any number of white spaces.
-}


view : String -> Html msg
view model =
    div [ class "jumbotron" ]
        [ h1 [] [ text "Welcome to Dunder Mifflin!" ]
        , p []
            [ text "Dunder Mifflin Inc. (stock symbol "
            , strong [] [ text "DMI" ]
            , text <|
                """
                ) is a micro-cap regional paper and office
                supply distributor with an emphasis on servicing
                small-business clients.
                """
            ]
        , p []
            [ Regex.contains regex apollo11
                |> fromBool
                |> (++) "Regex.contains regex apollo11"
                |> text
            ]
        , p []
            [ Regex.find newRegex newString
                |> List.map matchRecordToString
                |> String.join "; "
                |> text
            ]
        , p []
            [ Regex.replace newRegex (\_ -> "go-getter") newString
                |> text
            ]
        , Regex.split splitRegex apollo11
            |> List.map (\t -> p [] [ text t ])
            |> p []
        ]


main : Html msg
main =
    view "dummy model"


pattern : String
pattern =
    "\\d\\d:\\d\\d (a\\.m\\.|p\\.m\\.)"


maybeRegex : Maybe Regex
maybeRegex =
    Regex.fromString pattern


regex : Regex
regex =
    Maybe.withDefault Regex.never maybeRegex


apollo11 : String
apollo11 =
    """
            On July 16, 1969, the massive Saturn V rocket lifted off from NASA's Kennedy Space Center at 09:32 a.m. EDT.
            Four days later, on July 20, Neil Armstrong and BUzz Aldrin landed on the Moon. 
           """


fromBool : Bool -> String
fromBool value =
    if value then
        "True"

    else
        "False"



{-
   # Extracting a substring

   To extract a substring, we can use Regex.find function which is much more powerful than String.slice.
   Regex.find returns a list of records each containing information about the matches:
       * index: The index of the matched substring in the original string
       * match: The substring we are looking for.
       * number: If multiple substring are found, Regex.find labels each match with a number starting at one. The first
       match is labeled 1, the second is labeled 2, and so on. This number will be important when replacing all
       occurrences of a substring later.
       * submatches: Regex.find also looks for substrings that match any subpattern included inside the original pattern.
       In our case, substring "a.m." matches the subpattern (a\\.m\\.|p\\.m\\.). All submatches are wrapped inside
       Just.
-}


newPattern : String
newPattern =
    "quitter"


newRegex : Regex
newRegex =
    regexBuilder newPattern


regexBuilder : String -> Regex
regexBuilder patt =
    Regex.fromString patt |> Maybe.withDefault Regex.never


newString : String
newString =
    """
    I'm a great quitter. It's one of the few things I do well. I come from a long line of quitters. My father was a
    quitter, my grandfather was a quitter... I was raised to give up.
    """


matchRecordToString : Regex.Match -> String
matchRecordToString rec =
    String.concat
        [ "index: "
        , String.fromInt rec.index
        , ", match: "
        , rec.match
        , ", number: "
        , String.fromInt rec.number
        , ", submatches: "
        , "["
            ++ (String.join "," <|
                    List.map
                        (\val ->
                            case val of
                                Just m ->
                                    m

                                Nothing ->
                                    "[Empty]"
                        )
                        rec.submatches
               )
            ++ "]"
        ]


splitPattern : String
splitPattern =
    "\\d\\d:\\d\\d"


splitRegex : Regex
splitRegex =
    regexBuilder splitPattern
