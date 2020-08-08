module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (draggable, style)
import Html.Events exposing (on, preventDefaultOn)
import Json.Decode as Decode


type alias Element =
    String


type alias Model =
    { draggedIndex : Maybe Int
    , elements : Array Element
    }


initialModel : Model
initialModel =
    { draggedIndex = Nothing
    , elements =
        Array.fromList
            [ "0"
            , "1"
            , "2"
            , "3"
            , "4"
            , "5"
            , "6"
            , "7"
            , "8"
            , "9"
            , "10"
            ]
    }


type Msg
    = DragStart Element
    | DragEnd Element
    | DragOver
    | Drop
    | DragEnter Element


update : Msg -> Model -> Model
update msg model =
    case msg of
        DragStart element ->
            { model
                | draggedIndex = find (\elem -> elem == element) model.elements
            }

        DragEnter toElement ->
            let
                maybeFromIndex =
                    model.draggedIndex

                maybeToIndex =
                    find (\elem -> elem == toElement) model.elements
            in
            Maybe.withDefault model
                (Maybe.map2
                    (\fromIndex toIndex -> { draggedIndex = Just toIndex, elements = sort fromIndex toIndex model.elements })
                    maybeFromIndex
                    maybeToIndex
                )

        _ ->
            model


find : (a -> Bool) -> Array a -> Maybe Int
find predicate array =
    findInternal predicate 0 array


findInternal : (a -> Bool) -> Int -> Array a -> Maybe Int
findInternal predicate currentIndex array =
    let
        check =
            \currentValue ->
                if predicate currentValue then
                    Just currentIndex

                else
                    findInternal predicate (currentIndex + 1) array
    in
    Maybe.andThen check (Array.get currentIndex array)


type Direction
    = UP
    | DOWN


sort : Int -> Int -> Array a -> Array a
sort fromIndex toIndex array =
    let
        upperEnd =
            min fromIndex toIndex

        lowerStart =
            max (fromIndex + 1) (toIndex + 1)

        upper =
            Array.slice 0 upperEnd array

        lower =
            Array.slice lowerStart (Array.length array) array

        toRotate =
            Array.slice upperEnd lowerStart array

        middle =
            if fromIndex < toIndex then
                rotate toRotate UP

            else
                rotate toRotate DOWN
    in
    Array.append upper <| Array.append middle lower


rotate : Array a -> Direction -> Array a
rotate array direction =
    case direction of
        UP ->
            let
                length =
                    Array.length array

                toMove =
                    Array.slice 0 1 array

                others =
                    Array.slice 1 length array
            in
            Array.append others toMove

        DOWN ->
            let
                length =
                    Array.length array

                others =
                    Array.slice 0 (length - 1) array

                toMove =
                    Array.slice (length - 1) length array
            in
            Array.append toMove others


view : Model -> Html Msg
view model =
    div []
        (Array.toList (Array.map elementView model.elements))


elementView : Element -> Html Msg
elementView elem =
    div
        [ style "background-color" "coral"
        , style "margin" "15px"
        , style "max-width" "100px"
        , style "min-height" "30px"
        , draggable "true"
        , onDragStart <| DragStart elem
        , onDragEnd <| DragEnd elem
        , onDragOver DragOver
        , onDragEnter <| DragEnter elem
        , onDrop Drop
        ]
        [ text elem ]


onDragStart : Msg -> Attribute Msg
onDragStart msg =
    on "dragstart" <| Decode.succeed msg


onDragEnd : Msg -> Attribute Msg
onDragEnd msg =
    on "dragend" <| Decode.succeed msg


onDragOver : Msg -> Attribute Msg
onDragOver msg =
    preventDefaultOn "dragover" <| Decode.succeed ( msg, True )


onDragEnter : Msg -> Attribute Msg
onDragEnter msg =
    on "dragenter" <| Decode.succeed msg


onDrop : Msg -> Attribute Msg
onDrop msg =
    preventDefaultOn "drop" <| Decode.succeed ( msg, True )


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
