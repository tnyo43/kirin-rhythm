module ScorePanel exposing (Info, missCount, scorePanel)

import Lane exposing (Combo, comboToInt)
import Playground exposing (..)


type alias Info =
    { score : Int
    , combo : Combo
    }


scorePanel : Info -> List Shape -> List Shape
scorePanel memory lst =
    [ words
        black
        "current height : "
        |> scale 3
        |> moveY 450
    , words
        black
        ((memory.score |> String.fromInt) ++ "m")
        |> scale 10
        |> moveY 300

    ]
        |> List.map (moveX -500)
        |> (\shapes -> List.append shapes lst)


missCount : Int -> List Shape
missCount miss =
    let
        color n i =
            if i > n then
                red

            else
                black
    in
    List.range 0 2
        |> List.map
            (\n ->
                [ rectangle (color n miss) 30 150
                , rectangle (color n miss) 150 30
                ]
                    |> List.map (\s -> rotate 45 s |> move (140 * (n - 1) + 500 |> toFloat) -150)
            )
        |> List.concat
