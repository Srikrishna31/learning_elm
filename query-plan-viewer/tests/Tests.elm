module Tests exposing (..)

import Auth exposing (Msg(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode
import Main exposing (Msg(..), init, update)
import PlanParsers.Json exposing (SavedPlan, decodeSavedPlans)
import Test exposing (..)
import TestUpdate


suite : Test
suite =
    describe "Saved plan JSON decoder"
        [ test "Decoder converts valid strings" <|
            \_ ->
                let
                    json =
                        """
                        [
                            {"id":"1"
                            , "name": "Project query plan"
                            , "versions" : [ { "version": 1
                                            , "createdAt": "2017-12-16"
                                            , "planText" : "{}"
                                            }
                                            ]
                            }
                        ]
                        """

                    result =
                        Json.Decode.decodeString decodeSavedPlans json
                in
                Expect.equal result
                    (Ok
                        [ { id = "1"
                          , name = "Project query plan"
                          , versions =
                                [ { version = 1
                                  , createdAt = "2017-12-16"
                                  , planText = "{}"
                                  }
                                ]
                          }
                        ]
                    )
        , test "Decoder fails on missing fields" <|
            \_ ->
                let
                    json =
                        """
                            [{}]
                            """

                    result =
                        Json.Decode.decodeString decodeSavedPlans json
                in
                Expect.err result
        , test "Version order" <|
            \_ ->
                expectSortedVersions
                    { id = "1"
                    , name = "Plan"
                    , versions =
                        [ { version = 1, createdAt = "2017-12-01", planText = "{}" }
                        , { version = 3, createdAt = "2017-12-03", planText = "{}" }
                        , { version = 5, createdAt = "2017-12-02", planText = "{}" }
                        ]
                    }
        ]


suite1 : Test
suite1 =
    let
        ( model, _ ) =
            init { sessionId = Just "123" }
    in
    describe "Update function"
        [ TestUpdate.fuzz update
            msgFuzzer
            model
            "Should not reset session ID Wrongly"
          <|
            \mdl ->
                mdl.appState.auth.sessionId |> Expect.notEqual Nothing
        ]


expectSortedVersions : SavedPlan -> Expectation
expectSortedVersions plan =
    let
        versions =
            List.map .version plan.versions
    in
    if List.sort versions == versions then
        Expect.pass

    else
        Expect.fail "Versions are not sorted"


msgFuzzer : Fuzz.Fuzzer Main.Msg
msgFuzzer =
    Fuzz.oneOf <|
        List.map Fuzz.constant
            [ Auth <| ChangePassword "a"
            , Auth <| ChangeUserName "a"
            , Auth <| StartLogin
            , ChangePlanText "{}"
            , CreatePlan
            , RequestLogin
            , RequestRegistration
            , SubmitPlan
            , ToggleMenu
            ]
