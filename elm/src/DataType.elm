module DataType exposing (..)

import Html exposing (Html, div, pre, text)


type DataType a
    = IntData Int
    | FloatData Float
    | StringData String
    | TupleData (List (DataType a))
    | ListData (List (DataType a))
    | RecordData (List ( String, DataType a ))
    | CustomTypeData String (List (DataType a))


dataTypeToString dataType =
    case dataType of
        IntData value ->
            String.fromInt value

        FloatData value ->
            String.fromFloat value

        StringData value ->
            value

        TupleData value ->
            "(" ++ String.join "," (List.map dataTypeToString value) ++ ")"

        ListData value ->
            "[" ++ String.join "," (List.map dataTypeToString value) ++ "]"

        RecordData list ->
            "{" ++ String.join "," (List.map fieldDataTypeToString list) ++ "}"

        CustomTypeData name list ->
            name ++ " " ++ String.join " " (List.map dataTypeToString list)


fieldDataTypeToString : ( String, DataType a ) -> String
fieldDataTypeToString ( name, data ) =
    name ++ ": " ++ dataTypeToString data


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
    | RecordDiff (List ( String, Comparison a ))
    | CustomTypeNameDiff
        { expected : { name : String, data : List (DataType a) }
        , actual : { name : String, data : List (DataType a) }
        }
    | CustomTypeContentsDiff String (List (Comparison a))


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

        RecordDiff list ->
            String.join "\n" (List.map fieldDiffToString list)

        CustomTypeNameDiff { expected, actual } ->
            ("-" ++ expected.name ++ " " ++ (String.join "\n" <| List.map dataTypeToString expected.data))
                ++ "\n"
                ++ (" +" ++ actual.name ++ " " ++ (String.join "\n" <| List.map dataTypeToString actual.data))

        CustomTypeContentsDiff name diffs ->
            name ++ " " ++ String.join "\n" (List.map diffToString diffs)


fieldDiffToString : ( String, Comparison a ) -> String
fieldDiffToString ( name, diff_ ) =
    name ++ ": " ++ diffToString diff_


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

            ( RecordData e, RecordData a ) ->
                diffRecords e a

            ( CustomTypeData eName e, CustomTypeData aName a ) ->
                diffCustomTypes ( eName, e ) ( aName, a )

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

    else
        TupleDiff <| List.map2 diff e a


diffRecords : List ( String, DataType a ) -> List ( String, DataType a ) -> Comparison a
diffRecords e a =
    if List.map Tuple.first e /= List.map Tuple.first a then
        Error "Records have diffing number or order of fields"

    else
        RecordDiff <| List.map2 diffField e a


diffField : ( String, DataType a ) -> ( String, DataType a ) -> ( String, Comparison a )
diffField ( eName, e ) ( _, a ) =
    ( eName, diff e a )


diffCustomTypes : ( String, List (DataType a) ) -> ( String, List (DataType a) ) -> Comparison a
diffCustomTypes ( eName, e ) ( aName, a ) =
    if eName /= aName then
        CustomTypeNameDiff { expected = { name = eName, data = e }, actual = { name = aName, data = a } }

    else
        CustomTypeContentsDiff eName <| List.map2 diff e a


main : Html msg
main =
    let
        a =
            CustomTypeData "Bob" [ IntData 1, FloatData 2.0 ]

        b =
            CustomTypeData "Bob" [ IntData 1, FloatData 2.5 ]
    in
    div []
        [ text "Result: "
        , pre [] [ text (diff a b |> diffToString) ]
        ]
