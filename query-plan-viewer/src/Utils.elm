module Utils exposing (..)

import Http


httpErrorString : Http.Error -> String
httpErrorString err =
    case err of
        Http.BadBody message ->
            "Unable to handle response: " ++ message

        Http.BadUrl url ->
            "Invalid URL: " ++ url

        Http.Timeout ->
            "Request Timed out"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus statusCode ->
            "Server error: " ++ String.fromInt statusCode
