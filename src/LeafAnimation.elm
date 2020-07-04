module LeafAnimation exposing (Leaf, filterLeaves, initLeaves, movingLeaves)

import Background exposing (destination)
import Lane exposing (panelSize, panelScale)
import Playground exposing (..)


type alias Leaf =
    { x : Number
    , y : Number
    , scale : Number
    , startAt : Number
    }


destination : Int -> ( Number, Number )
destination score =
    ( -800, Background.destination score + 270 )


moveTime : Number
moveTime =
    3


initLeaves : Time -> List ( Number, Number ) -> List Leaf
initLeaves time nums =
    let
        startAt =
            spin moveTime time
    in
    List.map
        (\( x, y ) ->
            Leaf
                x
                y
                1
                startAt
        )
        nums


filterLeaves : Time -> List Leaf -> List Leaf
filterLeaves time leaves =
    List.filter
        (\leaf ->
            let
                past =
                    spin moveTime time
                        - leaf.startAt
                        |> floor
                        |> modBy 360
                        |> (+) 1

                ratio =
                    toFloat past / 360
            in
            ratio < 0.5
        )
        leaves


movingLeaves : Time -> Int -> List Leaf -> List Shape -> List Shape
movingLeaves time score leaves lst =
    List.map
        (\leaf ->
            let
                past =
                    spin moveTime time
                        - leaf.startAt
                        |> floor
                        |> modBy 360
                        |> (+) 1
                        |> toFloat

                ratio =
                    past / 360

                dest =
                    destination score

                x =
                    Tuple.first dest * ratio + leaf.x * (1 - ratio)
                y =
                    Tuple.second dest * ratio + leaf.y * (1 - ratio)

                size =
                    panelSize * panelScale

            in
            image
                size
                size
                "/assets/leaf.png"
                |> fade 0.5
                |> move x y
        )
        leaves
    |> (\shapes -> shapes ++ lst)