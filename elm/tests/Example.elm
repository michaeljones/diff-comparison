module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Tests"
        [ test "diff two records" <|
            \() ->
                let
                    a =
                        { first = 1
                        , second = 2
                        , third = 3
                        }

                    b =
                        { first = 1
                        , second = 4
                        , third = 3
                        }
                in
                a |> Expect.equal b
        , test "diff two awkward records" <|
            \() ->
                let
                    a =
                        { just = Just 1
                        , nothing = Nothing
                        , otherJust = Just 2
                        }

                    b =
                        { just = Nothing
                        , nothing = Just 3
                        , otherJust = Nothing
                        }

                    c =
                        { just = Just a
                        , nothing = Nothing
                        , otherJust = Just b
                        }

                    d =
                        { just = Nothing
                        , nothing = Just b
                        , otherJust = Just a
                        }
                in
                c |> Expect.equal d
        ]
