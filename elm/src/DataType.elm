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
    | IntSame Int
    | IntDiff { expected : Int, actual : Int }
    | FloatSame Float
    | FloatDiff { expected : Float, actual : Float }
    | StringSame String
    | StringDiff { expected : String, actual : String }
    | TupleSame (List (DataType a))
    | TupleDiff (List (Comparison a))
    | ListSame (List (DataType a))
    | ListDiff (List (Comparison a))


isDiff : Comparison a -> Bool
isDiff comp =
    case comp of
        Plus _ ->
            True

        Minus _ ->
            True

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

        ListSame _ ->
            False

        ListDiff _ ->
            True

        Error _ ->
            True


diffToString : Comparison a -> String
diffToString diff_ =
    case diff_ of
        Error string ->
            "Error: " ++ string

        Plus value ->
            "+" ++ dataTypeToString value

        Minus value ->
            "-" ++ dataTypeToString value

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
            String.join "\n" (List.map dataTypeToString diffs)

        TupleDiff diffs ->
            String.join "\n" (List.map diffToString diffs)

        ListSame diffs ->
            String.join "\n" (List.map dataTypeToString diffs)

        ListDiff diffs ->
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
        ListSame e


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
        TupleSame e


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
