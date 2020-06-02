module Main exposing (main)

import Animation exposing (..)
import Game exposing (Data, clock, initData)
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
            []
        )



-- UPDATE


type alias Memory =
    { score : Int
    , combo : Combo
    , lanes : List Lane
    , data : Data
    , leaves : List Leaf
    }


update : Computer -> Memory -> Memory
update computer memory =
    let
        ( ls, ( newScore, newCombo ), leaves ) =
            List.map2 Tuple.pair addNow memory.lanes
                |> List.map
                    (\( addnow, lane ) ->
                        (if addnow then
                            addNote lane

                         else
                            lane
                        )
                            |> press (Set.member lane.key computer.keyboard.keys)
                    )
                |> Lane.step
                |> (\( lane_, ( score, combo ), leaves_ ) ->
                        ( lane_, ( score + memory.score, addCombo combo memory.combo ), initLeaves computer.time leaves_ )
                   )

        updatedLeaves =
            Animation.filterLeaves
                computer.time
                memory.leaves
                |> (++) leaves

        ( updatedData, addNow ) =
            clock computer.time memory.data
    in
    { memory
        | score = newScore
        , combo = resetComboIfCut newCombo
        , lanes = ls
        , data = updatedData
        , leaves = updatedLeaves
    }



-- VIEW


view : Computer -> Memory -> List Shape
view computer memory =
    ([])
        |> movingLeaves computer.time memory.leaves
        |> scorePanel { score = memory.score, combo = memory.combo }
        |> lanes memory.lanes
