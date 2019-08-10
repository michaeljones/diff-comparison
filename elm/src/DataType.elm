module DataType exposing (..)

import Html exposing (Html, div, pre, text)


type DataType a
    = IntData Int
    | FloatData Float
    | StringData String
    | TupleData (List (DataType a))
    | ListData (List (DataType a))
    | RecordData (List ( String, DataType a ))


type Comparison a
    = IntSame Int
    | IntDiff { expected : Int, actual : Int }
    | FloatSame Float
    | FloatDiff { expected : Float, actual : Float }
    | StringSame String
    | StringDiff { expected : String, actual : String }
    | TupleSame (List (Comparison a))
    | TupleDiff (List (Comparison a))
    | Error String


isDiff : Comparison a -> Bool
isDiff comp =
    case comp of
        IntSame _ ->
            False

        IntDiff _ ->
            True

        FloatSame _ ->
            False

        FloatDiff _ ->
            True

        StringSame _ ->
            False

        StringDiff _ ->
            True

        TupleSame _ ->
            False

        TupleDiff _ ->
            True

        Error _ ->
            True


diffToString : Comparison a -> String
diffToString diff_ =
    case diff_ of
        Error string ->
            "Error: " ++ string

        IntSame value ->
            String.fromInt value

        IntDiff { expected, actual } ->
            "-" ++ String.fromInt expected ++ "\n+" ++ String.fromInt actual

        FloatSame value ->
            String.fromFloat value

        FloatDiff { expected, actual } ->
            "-" ++ String.fromFloat expected ++ "\n+" ++ String.fromFloat actual

        StringSame value ->
            "\"" ++ value ++ "\""

        StringDiff { expected, actual } ->
            "-\"" ++ expected ++ "\"\n+\"" ++ actual ++ "\""

        TupleSame diffs ->
            String.join "\n" (List.map diffToString diffs)

        TupleDiff diffs ->
            String.join "\n" (List.map diffToString diffs)


diff : DataType a -> DataType a -> Comparison a
diff expected actual =
    case ( expected, actual ) of
        ( IntData e, IntData a ) ->
            if e /= a then
                IntDiff { expected = e, actual = a }

            else
                IntSame e

        ( FloatData e, FloatData a ) ->
            if e /= a then
                FloatDiff { expected = e, actual = a }

            else
                FloatSame e

        ( StringData e, StringData a ) ->
            if e /= a then
                StringDiff { expected = e, actual = a }

            else
                StringSame e

        ( TupleData e, TupleData a ) ->
            let
                eLength =
                    List.length e
            in
            if eLength /= List.length a then
                Error "Tuples of different length"

            else if eLength < 2 || eLength > 4 then
                Error "Tuples must have 2 to 4 elements"

            else
                let
                    elements =
                        List.map2 diff e a
                in
                if List.any isDiff elements then
                    TupleDiff elements

                else
                    TupleSame elements

        _ ->
            Error "Comparing different data types"


main : Html msg
main =
    let
        a =
            TupleData [ IntData 1, IntData 2 ]

        b =
            TupleData [ IntData 3, IntData 2 ]
    in
    div []
        [ text "Result: "
        , pre [] [ text (diff a b |> diffToString) ]
        ]
