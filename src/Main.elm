module Main exposing (main)

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
            0
        )



-- UPDATE


type alias Memory =
    { score : Int
    , combo : Combo
    , lanes : List Lane
    , clock : Number
    }


update : Computer -> Memory -> Memory
update computer memory =
    let
        ( ls, newScore, newCombo ) =
            List.map
                (\lane ->
                    (if currentClock < memory.clock then
                        addNote lane

                     else
                        lane
                    )
                        |> press (Set.member lane.key computer.keyboard.keys)
                )
                memory.lanes
                |> step
                |> (\( lane_, score, combo ) ->
                        ( lane_, score + memory.score, addCombo combo memory.combo )
                   )

        currentClock =
            spin 3 computer.time
    in
    { memory
        | score = newScore
        , combo = resetComboIfCut newCombo
        , lanes = ls
        , clock = currentClock
    }



-- VIEW


view : Computer -> Memory -> List Shape
view _ memory =
    []
        |> scorePanel { score = memory.score, combo = memory.combo }
        |> lanes memory.lanes
