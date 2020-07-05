module Background exposing (destination, kirin, wholeKirin)

import Playground exposing (..)


scoreThre : Int
scoreThre =
    100


destination : Int -> Number
destination score =
    score
        - scoreThre
        |> min 0
        |> (*) 5
        |> toFloat
        |> (+) -100


kirinFace : List Shape
kirinFace =
    [ rectangle yellow 200 100 -- face
    , rectangle orange 40 100 |> moveX 80 -- nose
    , rectangle black 20 1 |> rotate 130 |> move 80 35 -- nose
    , oval red 60 40 |> move 60 -50 -- mouse
    , rectangle yellow 60 40 |> move -100 35 -- ear
    , rectangle yellow 20 50 |> move -80 70 -- horn
    , circle brown 20 |> move -80 80 -- horn
    , circle black 20 |> moveX -20 -- eye
    ]


kirinNeck : Int -> List Shape
kirinNeck score =
    let
        sc =
            toFloat score * 5
    in
    List.range
        0
        (score // 20)
        |> List.map
            (\i ->
                oval brown 70 50
                    |> move ((1 - modBy 2 i) |> (*) 20 |> (-) 10 |> toFloat) (100 * i - score * 5 |> toFloat)
            )
        |> (\l ->
                (rectangle yellow 100 sc |> moveY -(sc / 2 + 50))
                    :: l
                    |> List.map (moveX -40)
           )


kirinBody : Int -> List Shape
kirinBody score =
    [ rectangle yellow 300 150 -- body
    , rectangle yellow 30 120 |> move 135 -135
    , rectangle yellow 30 120 |> move 100 -135
    , rectangle yellow 30 120 |> move -100 -135
    , rectangle yellow 30 120 |> move -135 -135 -- legs
    , oval brown 70 50
    , oval brown 70 50 |> move 100 40
    , oval brown 70 50 |> move 100 -40
    , oval brown 70 50 |> move -100 40
    , oval brown 70 50 |> move -100 -40
    ]
        |> List.map (move -100 -( 5 * (toFloat score) + 120))


baseKirin : Int -> List Shape
baseKirin score =
    kirinBody score
        |> List.map (moveX -40)
        |> \body -> kirinNeck score ++ body ++ kirinFace


kirin : Int -> List Shape -> List Shape
kirin score lst =
    let
        k =
            baseKirin score
                |> List.map (move -700 (destination score + 250))
    in
    k ++ lst


wholeKirin : Int -> Number -> List Shape -> List Shape
wholeKirin score y lst =
    let
        s =
            toFloat score |> (*) 5

        k =
            baseKirin score
             |> List.map (moveY (s + y))
    in
    k ++ lst
