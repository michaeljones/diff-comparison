module DataType exposing (..)

import Html exposing (Html, div, pre, text)


type DataType a
    = IntData Int
    | FloatData Float
    | StringData String
    | TupleData (List (DataType a))
    | ListData (List (DataType a))
    | RecordData (List ( String, DataType a ))


dataTypeToString dataType =
    case dataType of
        IntData value ->
            String.fromInt value

        FloatData value ->
            String.fromFloat value

        StringData value ->
            value

        TupleData value ->
            String.join "," (List.map dataTypeToString value)

        ListData value ->
            String.join "," (List.map dataTypeToString value)

        RecordData value ->
            "Record not implemented"


type Comparison a
    = Error String
    | Plus (DataType a)
    | Minus (DataType a)
    | Same (DataType a)
    | IntDiff { expected : Int, actual : Int }
    | FloatDiff { expected : Float, actual : Float }
    | StringDiff { expected : String, actual : String }
    | TupleDiff (List (Comparison a))
    | ListDiff (List (Comparison a))


isDiff : Comparison a -> Bool
isDiff comp =
    case comp of
        Same _ ->
            False

        _ ->
            True


diffToString : Comparison a -> String
diffToString diff_ =
    case diff_ of
        Error string ->
            "Error: " ++ string

        Same value ->
            dataTypeToString value

        Plus value ->
            "+" ++ dataTypeToString value

        Minus value ->
            "-" ++ dataTypeToString value

        IntDiff { expected, actual } ->
            "-" ++ String.fromInt expected ++ "\n+" ++ String.fromInt actual

        FloatDiff { expected, actual } ->
            "-" ++ String.fromFloat expected ++ "\n+" ++ String.fromFloat actual

        StringDiff { expected, actual } ->
            "-\"" ++ expected ++ "\"\n+\"" ++ actual ++ "\""

        TupleDiff diffs ->
            String.join "\n" (List.map diffToString diffs)

        ListDiff diffs ->
            String.join "\n" (List.map diffToString diffs)


diff : DataType a -> DataType a -> Comparison a
diff expected actual =
    if expected == actual then
        Same expected

    else
        case ( expected, actual ) of
            ( IntData e, IntData a ) ->
                IntDiff { expected = e, actual = a }

            ( FloatData e, FloatData a ) ->
                FloatDiff { expected = e, actual = a }

            ( StringData e, StringData a ) ->
                StringDiff { expected = e, actual = a }

            ( TupleData e, TupleData a ) ->
                diffTuples e a

            ( ListData e, ListData a ) ->
                diffLists e a

            _ ->
                Error "Comparing different data types"


longZip : (Maybe a -> Maybe b -> c) -> List a -> List b -> List c
longZip fn listA listB =
    longZipHelper fn listA listB []
        |> List.reverse


longZipHelper fn listA listB acc =
    case ( listA, listB ) of
        ( [], [] ) ->
            acc

        ( a :: aRest, [] ) ->
            longZipHelper fn aRest [] (fn (Just a) Nothing :: acc)

        ( [], b :: bRest ) ->
            longZipHelper fn [] bRest (fn Nothing (Just b) :: acc)

        ( a :: aRest, b :: bRest ) ->
            longZipHelper fn aRest bRest (fn (Just a) (Just b) :: acc)


diffLists e a =
    if e /= a then
        ListDiff <| longZip diffMaybe e a

    else
        Same <| ListData e


diffMaybe e a =
    case ( e, a ) of
        ( Nothing, Nothing ) ->
            Error "Two nothings in list comparison"

        ( Just eV, Nothing ) ->
            Minus eV

        ( Nothing, Just aV ) ->
            Plus aV

        ( Just eV, Just aV ) ->
            diff eV aV


diffTuples : List (DataType a) -> List (DataType a) -> Comparison a
diffTuples e a =
    let
        eLength =
            List.length e
    in
    if eLength /= List.length a then
        Error "Tuples of different length"

    else if eLength < 2 || eLength > 4 then
        Error "Tuples must have 2 to 4 elements"

    else if e /= a then
        TupleDiff <| List.map2 diff e a

    else
        Same <| TupleData e


main : Html msg
main =
    let
        a =
            ListData [ IntData 1, IntData 2, IntData 5 ]

        b =
            ListData [ IntData 3, IntData 2 ]
    in
    div []
        [ text "Result: "
        , pre [] [ text (diff a b |> diffToString) ]
        ]
