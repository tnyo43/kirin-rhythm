module Main exposing (main)

import Array exposing (Array)
import Playground exposing (..)
import Set exposing (member)


-- MAIN

main =
  game
    view
    update
    ( Memory
        0
        (Array.repeat 4 False)
        (Array.repeat 4 Array.empty)
        0
    )



-- UPDATE

type alias Memory =
    { score : Int
    , mentionedKeysPressed : Array Bool
    , notes : Array (Array Number)
    , clock : Number
    }


mentionedKeys : Array String
mentionedKeys = Array.fromList ["e", "f", "j", "o"]

mentionedKeysLen : Int
mentionedKeysLen = Array.length mentionedKeys

noteIn : Number
noteIn = 1000

noteOut : Number
noteOut = -100

update : Computer -> Memory -> Memory
update computer memory =
    let
        pressed =
            Array.foldl
                (\(i, s) arr-> Array.set i (Set.member s computer.keyboard.keys) arr)
                ( Array.repeat (Array.length mentionedKeys) False )
                ( Array.indexedMap (\i s -> (i, s)) mentionedKeys )
        currentClock = spin 3 computer.time
        curretNotes =
            Array.map
                (\lane_ ->
                    if currentClock < memory.clock
                        then Array.push noteIn lane_
                        else lane_
                    |> Array.map ((+) -1)
                    |>  Array.filter (\x -> x > noteOut)
                )
                memory.notes
    in
    { memory
        | mentionedKeysPressed = pressed
        , clock = currentClock
        , notes = curretNotes
    }


-- VIEW


panelSize : Number
panelSize = 25

panelHeight : Number
panelHeight = -250

panelInterval : Number
panelInterval = 150

laneWidth : Number
laneWidth = 20

panelScale : Number
panelScale = 5

notePanel : List Shape
notePanel =
    let
        border = 4
    in
    [ rectangle grey panelSize border |> moveY ((panelSize*2+border))
    , rectangle grey panelSize border |> moveY (-(panelSize*2+border))
    , rectangle grey border panelSize |> moveX ((panelSize*2+border))
    , rectangle grey border panelSize |> moveX (-(panelSize*2+border))
    ]


lane : Memory -> Int -> List Shape
lane memory idx =
    [ rectangle lightGrey laneWidth 400
    , square
        ( if Array.get idx memory.mentionedKeysPressed |> Maybe.withDefault False
            then red
            else blue
        )
        panelSize
    , words
        white
        ( Array.get idx mentionedKeys |> Maybe.withDefault "" |> String.toUpper )
    ]
    ++
    ( Array.get idx memory.notes
        |> Maybe.withDefault Array.empty
        |> Array.map
                (\y -> notePanel |> List.map (moveY y) )
        |> Array.toList
        |> List.concat
    )
        |> List.map
            ( move ( toFloat ( 2 * idx - mentionedKeysLen + 1 ) / 2
                |> (*) panelInterval) panelHeight
            )

view : Computer -> Memory -> List Shape
view _ memory =
    List.range 0 (mentionedKeysLen - 1)
        |> List.map (lane memory)
        |> List.concat
        |> List.map (scale panelScale)
