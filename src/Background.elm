module Background exposing (destination, kirin)

import Playground exposing (..)

scoreThre : Int
scoreThre =
    30


destination : Int -> Number
destination score =
    scoreThre - score
        |> max 0
        |> (*) -10
        |> toFloat
        |> (+) -250


kirin : Int -> List Shape -> List Shape
kirin score lst =
    let
        face =
            [ rectangle yellow 200 100 -- face
            , rectangle orange 40 100 |> moveX 80 -- nose
            , rectangle black 20 1 |> rotate 130 |> move 80 35 -- nose
            , oval red 60 40 |> move 60 -50 -- mouse
            , rectangle yellow 60 40 |> move -100 35 -- ear
            , rectangle yellow 20 50 |> move -80 70 -- horn
            , circle brown 20 |> move -80 80 -- horn
            , circle black 20 |> moveX -20 -- eye
            ]

        neck =
            modBy 25 score
            |> (*) 4
            |> \s ->
                List.range (if s >= 50 then 0 else 1) 10
                |> List.map (\i ->
                    oval brown 70 50
                    |> move ((1 - modBy 2 i) |> (*) 20 |> (-) 10 |> toFloat) (50*i-s |> toFloat))
            |> \l -> rectangle yellow 100 1000 :: l
            |> List.map (move -40 -500)
            
        k =
            neck ++ face 
            |> List.map (move -700 (destination score + 250))
    in

    k ++ lst
