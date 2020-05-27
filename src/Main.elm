module Main exposing (main)

import Array exposing (Array)
import Playground exposing (..)
import Set exposing (member)



-- MAIN


main =
    game
        view
        update
        (Memory
            0
            0
            (Array.repeat 4 False)
            (Array.repeat 4 Array.empty)
            0
        )



-- UPDATE


type alias Memory =
    { score : Int
    , combo : Int
    , mentionedKeysPressed : Array Bool
    , notes : Array (Array Number)
    , clock : Number
    }


mentionedKeys : Array String
mentionedKeys =
    Array.fromList [ "e", "f", "j", "o" ]


mentionedKeysLen : Int
mentionedKeysLen =
    Array.length mentionedKeys


noteIn : Number
noteIn =
    1000


speed : Number
speed =
    10


okScore : Int
okScore =
    1


greatScore : Int
greatScore =
    4


update : Computer -> Memory -> Memory
update computer memory =
    let
        pressed =
            Array.foldl
                (\( i, s ) arr -> Array.set i (Set.member s computer.keyboard.keys) arr)
                (Array.repeat (Array.length mentionedKeys) False)
                (Array.indexedMap (\i s -> ( i, s )) mentionedKeys)

        currentClock =
            spin 3 computer.time

        ( curretNotes, score, combo ) =
            Array.indexedMap
                (\idx lane_ ->
                    (if currentClock < memory.clock then
                        Array.push noteIn lane_

                     else
                        lane_
                    )
                        |> Array.map ((+) -speed)
                        |> (\l ->
                                ( Array.filter (\x -> x > -okFrame) l
                                , Array.toList l |> List.any (\x -> x <= -okFrame)
                                )
                                    |> (\( editLane, isComboCut ) ->
                                            if Array.get idx pressed |> Maybe.withDefault False |> not then
                                                ( editLane
                                                , 0
                                                , if isComboCut then
                                                    -100

                                                  else
                                                    0
                                                )

                                            else
                                                ( Array.filter (\x -> abs x > okFrame) editLane
                                                , (Array.filter (\x -> abs x <= okFrame) editLane |> Array.length |> (*) okScore)
                                                    + (Array.filter (\x -> abs x <= greatFrame) editLane |> Array.length |> (*) greatScore)
                                                , if isComboCut then
                                                    -100

                                                  else
                                                    Array.filter (\x -> abs x <= okFrame) editLane |> Array.length
                                                )
                                       )
                           )
                )
                memory.notes
                |> (\ls ->
                        ( Array.map (\( ns, _, _ ) -> ns) ls
                        , Array.map (\( _, s, _ ) -> s) ls |> Array.foldl (+) memory.score
                        , Array.map (\( _, _, c ) -> c) ls |> Array.foldl (+) memory.combo |> max 0
                        )
                   )
    in
    { memory
        | mentionedKeysPressed = pressed
        , clock = currentClock
        , notes = curretNotes
        , score = score
        , combo = combo
    }



-- VIEW


panelSize : Number
panelSize =
    25


panelHeight : Number
panelHeight =
    -250


panelInterval : Number
panelInterval =
    150


laneWidth : Number
laneWidth =
    20


panelScale : Number
panelScale =
    5


greatFrame : Number
greatFrame =
    20


okFrame : Number
okFrame =
    greatFrame * 3


notePanel : List Shape
notePanel =
    let
        border =
            4
    in
    [ rectangle grey panelSize border |> moveY (panelSize * 2 + border)
    , rectangle grey panelSize border |> moveY -(panelSize * 2 + border)
    , rectangle grey border panelSize |> moveX (panelSize * 2 + border)
    , rectangle grey border panelSize |> moveX -(panelSize * 2 + border)
    ]


lane : Memory -> Int -> List Shape
lane memory idx =
    [ rectangle lightGrey laneWidth 400
    , square
        (if Array.get idx memory.mentionedKeysPressed |> Maybe.withDefault False then
            red

         else
            blue
        )
        panelSize
    , words
        white
        (Array.get idx mentionedKeys |> Maybe.withDefault "" |> String.toUpper)
    ]
        ++ (Array.get idx memory.notes
                |> Maybe.withDefault Array.empty
                |> Array.map
                    (\y -> notePanel |> List.map (moveY y))
                |> Array.toList
                |> List.concat
           )
        |> List.map
            (move
                (toFloat (2 * idx - mentionedKeysLen + 1)
                    / 2
                    |> (*) panelInterval
                )
                panelHeight
            )


lanes : Memory -> List Shape -> List Shape
lanes memory lst =
    List.range 0 (mentionedKeysLen - 1)
        |> List.map (lane memory)
        |> List.concat
        |> List.map (scale panelScale)
        |> (\shapes -> List.append shapes lst)


scorePanel : Memory -> List Shape -> List Shape
scorePanel memory lst =
    [ words
        black
        "score"
        |> scale 6
        |> moveY 350
    , words
        black
        (memory.score |> String.fromInt)
        |> scale 10
        |> moveY 200
    , words
        black
        "combo"
        |> scale 6
        |> moveY 50
    , words
        black
        (memory.combo |> String.fromInt)
        |> scale 10
        |> moveY -100
    ]
        |> List.map (moveX 500)
        |> (\shapes -> List.append shapes lst)


view : Computer -> Memory -> List Shape
view _ memory =
    []
        |> scorePanel memory
        |> lanes memory
