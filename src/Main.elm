module Main exposing (main)

import Game exposing (Data, initData, clock)
import Lane exposing (Combo(..), Lane, addCombo, addNote, initLanes, lanes, press, resetComboIfCut, step)
import Playground exposing (..)
import ScorePanel exposing (scorePanel)
import Set exposing (member)



-- MAIN


main =
    game
        view
        update
        (Memory
            0
            (Continue 0)
            initLanes
            initData
        )



-- UPDATE


type alias Memory =
    { score : Int
    , combo : Combo
    , lanes : List Lane
    , data : Data
    }


update : Computer -> Memory -> Memory
update computer memory =
    let
        ( ls, newScore, newCombo ) =
            List.map2 Tuple.pair addNow memory.lanes
            |>
                List.map
                    (\( addnow, lane ) ->
                        (if addnow then
                            addNote lane

                        else
                            lane
                        )
                            |> press (Set.member lane.key computer.keyboard.keys)
                    )
            |> step
            |> (\( lane_, score, combo ) ->
                    ( lane_, score + memory.score, addCombo combo memory.combo )
                )

        ( updatedData, addNow ) =
            clock computer.time memory.data
    in
    { memory
        | score = newScore
        , combo = resetComboIfCut newCombo
        , lanes = ls
        , data = updatedData
    }



-- VIEW


view : Computer -> Memory -> List Shape
view _ memory =
    []
        |> scorePanel { score = memory.score, combo = memory.combo }
        |> lanes memory.lanes
