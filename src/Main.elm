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
        (Array.repeat 4 False)
    )



-- UPDATE


type alias Memory =
    { mentionedKeysPressed : Array Bool
    }



mentionedKeys : Array String
mentionedKeys = Array.fromList ["e", "f", "j", "o"]


update : Computer -> Memory -> Memory
update computer memory =
    let
        pressed =
            Array.foldl
                (\(i, s) arr-> Array.set i (Set.member s computer.keyboard.keys) arr)
                ( Array.repeat (Array.length mentionedKeys) False )
                ( Array.indexedMap (\i s -> (i, s)) mentionedKeys )
    in
    { memory | mentionedKeysPressed = pressed }


-- VIEW



panel : Memory -> Int -> List Shape
panel memory idx =
    let w = 150 in
    [ square
        (if Array.get idx memory.mentionedKeysPressed |> Maybe.withDefault False then red else blue)
        25
    , words
        white
        (Array.get idx mentionedKeys |> Maybe.withDefault "")
    ]
    |> List.map (moveX (toFloat idx - 1.5 |> (*) w))

view : Computer -> Memory -> List Shape
view _ memory =
    List.range 0 3
        |> List.map (panel memory)
        |> List.concat
        |> List.map (scale 5)
