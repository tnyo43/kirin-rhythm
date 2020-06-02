module Animation exposing (..)

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

step : Time -> List Leaf -> List Leaf
step time leaves =
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
            in
            { leaf | x = (Tuple.first destination - leaf.x) * ratio + leaf.x, y = (Tuple.second destination - leaf.y) * ratio + leaf.y, scale = leaf.scale * (1 - ratio) }
                |> Just
        )
        leaves
        |> List.foldl
            (\l lst ->
                case l of
                    Just le ->
                        le :: lst

                    _ ->
                        lst
            )
            []

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

            in
            image
                100
                100
                "/assets/leaf_lightgreen.png"
                |> move x y
        )
        leaves
    |> (\shapes -> shapes ++ lst)