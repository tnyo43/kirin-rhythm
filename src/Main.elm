module Main exposing (main)

import Background exposing (destination, kirin)
import Game exposing (Data, clock, initData)
import Lane exposing (Combo(..), Lane, addCombo, addNote, initLanes, lanes, press, resetComboIfCut, step)
import LeafAnimation exposing (..)
import Playground exposing (..)
import ScorePanel exposing (scorePanel, missCount)
import Set exposing (member)



-- MAIN


main =
    game
        view
        update
        (Memory
            Title
            0
            (Continue 0)
            initLanes
            initData
            []
            0
        )



-- UPDATE


type Scene
    = Title
    | Game
    | Ending


type alias Memory =
    { scene : Scene
    , score : Int
    , combo : Combo
    , lanes : List Lane
    , data : Data
    , leaves : List Leaf
    , miss : Int
    }


initGame : Memory -> Memory
initGame memory =
    { memory
        | scene = Game
        , score = 0
        , combo = Continue 0
        , lanes = initLanes
        , data = initData
        , leaves = []
        , miss = 0
    }


endGame : Memory -> Memory
endGame memory =
    { memory | scene = Ending }


updateTitle : Computer -> Memory -> Memory
updateTitle computer memory =
    if computer.keyboard.space then
        initGame memory

    else
        memory


updateGame : Computer -> Memory -> Memory
updateGame computer memory =
    let
        ( ls, ( newScore, newCombo, newMiss ), leaves ) =
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
                |> step
                |> (\( lane_, ( score, combo, miss ), leaves_ ) ->
                        ( lane_, ( score + memory.score, addCombo combo memory.combo, miss + memory.miss ), initLeaves computer.time leaves_ )
                   )

        updatedLeaves =
            filterLeaves
                computer.time
                memory.leaves
                |> (++) leaves

        ( updatedData, addNow ) =
            clock computer.time memory.data
    in
    if newMiss >= 3 then
        endGame memory

    else
        { memory
            | score = newScore
            , combo = resetComboIfCut newCombo
            , lanes = ls
            , data = updatedData
            , leaves = updatedLeaves
            , miss = newMiss
        }


updateEnding : Computer -> Memory -> Memory
updateEnding computer memory =
    if computer.keyboard.space then
        initGame memory

    else
        memory


update : Computer -> Memory -> Memory
update computer memory =
    case memory.scene of
        Title ->
            updateTitle computer memory

        Game ->
            updateGame computer memory

        Ending ->
            updateEnding computer memory



-- VIEW


viewTitle : Memory -> List Shape
viewTitle memory =
    [ words black "Feed your giraff"
        |> scale 4
    , words black "Play with your [E], [F], [J], [O] keys"
        |> moveY -50
        |> scale 2
    , words black "space to restart"
        |> moveY -80
        |> scale 2
    ]


viewGame : Computer -> Memory -> List Shape
viewGame computer memory =
    missCount memory.miss
        |> kirin memory.score
        |> movingLeaves computer.time memory.score memory.leaves
        |> scorePanel { score = memory.score, combo = memory.combo }
        |> lanes memory.lanes


viewEnding : Memory -> List Shape
viewEnding memory =
    [ words black ("Your Giraff grow to " ++ (memory.score |> String.fromInt) ++ " meters!!")
        |> scale 4
    , words black "space to restart"
        |> moveY -50
        |> scale 2
    ]


view : Computer -> Memory -> List Shape
view computer memory =
    case memory.scene of
        Title ->
            viewTitle memory

        Game ->
            viewGame computer memory

        Ending ->
            viewEnding memory
