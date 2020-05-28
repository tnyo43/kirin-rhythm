module ScorePanel exposing (Info, scorePanel)

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
        "score"
        |> scale 6
        |> moveY 450
    , words
        black
        (memory.score |> String.fromInt)
        |> scale 10
        |> moveY 300
    , words
        black
        "combo"
        |> scale 6
        |> moveY 150
    , words
        black
        (memory.combo |> comboToInt |> String.fromInt)
        |> scale 10
    ]
        |> List.map (moveX 500)
        |> (\shapes -> List.append shapes lst)
