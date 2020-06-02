module LeafAnimation exposing (Leaf, filterLeaves, initLeaves, movingLeaves)

import Lane exposing (panelSize, panelScale)
import Playground exposing (..)


type alias Leaf =
    { x : Number
    , y : Number
    , scale : Number
    , startAt : Number
    }


destination : ( Number, Number )
destination =
    ( -800, -0 )


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


movingLeaves : Time -> List Leaf -> List Shape -> List Shape
movingLeaves time leaves lst =
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

                x =
                    Tuple.first destination * ratio + leaf.x * (1 - ratio)
                y =
                    Tuple.second destination * ratio + leaf.y * (1 - ratio)

                size =
                    panelSize * panelScale

            in
            image
                size
                size
                "/assets/leaf_lightgreen.png"
                |> move x y
        )
        leaves
    |> (\shapes -> shapes ++ lst)