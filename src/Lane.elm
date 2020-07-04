module Lane exposing (Combo(..), Lane, addCombo, addNote, comboToInt, initLanes, lanes, press, resetComboIfCut, step, panelSize, panelScale)

import Array exposing (Array)
import Playground exposing (..)


type alias Note =
    { pos : Number
    }


type alias Lane =
    { id : Int
    , pressed : Bool
    , notes : Array Note
    , key : String
    }


initLanes : List Lane
initLanes =
    List.indexedMap
        (\i key -> Lane i False Array.empty key)
        mentionedKeys


type Combo
    = Continue Int
    | Cut


addCombo : Combo -> Combo -> Combo
addCombo c1 c2 =
    case ( c1, c2 ) of
        ( Continue n1, Continue n2 ) ->
            n1 + n2 |> Continue

        _ ->
            Cut


resetComboIfCut : Combo -> Combo
resetComboIfCut c =
    case c of
        Cut ->
            Continue 0

        _ ->
            c


comboToInt : Combo -> Int
comboToInt c =
    case c of
        Continue n ->
            n

        Cut ->
            0



-- const KEY


mentionedKeys : List String
mentionedKeys =
    [ "e", "f", "j", "o" ]


mentionedKeysLen : Int
mentionedKeysLen =
    List.length mentionedKeys



-- const GRAPHICS


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



-- const TIMING


greatFrame : Number
greatFrame =
    20


okFrame : Number
okFrame =
    greatFrame * 3


noteIn : Number
noteIn =
    1000


noteOut : Number
noteOut =
    -100


speed : Number
speed =
    10



-- const SCORE


okScore : Int
okScore =
    1


greatScore : Int
greatScore =
    4


notePanel : List Shape
notePanel =
    [ image panelSize panelSize "/assets/leaf.png"
    ]


addNote : Lane -> Lane
addNote l =
    { l | notes = Array.push { pos = noteIn } l.notes }


press : Bool -> Lane -> Lane
press pressed l =
    { l | pressed = pressed }


step : List Lane -> ( List Lane, (Int, Combo, Int), List ( Number, Number ) )
step ls =
    let
        filterOut ns =
            Array.filter (\n -> n.pos > noteOut) ns

        pressedNotes =
            List.concatMap
                (\lane_ ->
                    let
                        xOf =
                            xOfLane lane_
                    in

                    if lane_.pressed then
                        Array.filter (\n -> n.pos < okFrame) lane_.notes
                            |> Array.map (\y -> ( xOf, y.pos + panelHeight ))
                            |> Array.toList

                    else
                        []
                )

        filterPress pressed ns =
            if pressed then
                Array.filter (\n -> n.pos >= okFrame) ns

            else
                ns

        getCombo pressed ns =
            if Array.filter (\n -> n.pos <= noteOut) ns |> Array.isEmpty |> not then
                Cut

            else if pressed then
                Array.filter (\n -> abs n.pos < okFrame) ns |> Array.length |> Continue

            else
                Continue 0

        getScore pressed ns =
            if pressed then
                (Array.filter (\n -> abs n.pos < okFrame) ns |> Array.length |> (*) okScore)
                    + (Array.filter (\n -> abs n.pos < greatFrame) ns |> Array.length |> (*) greatScore)

            else
                0

        moveNotes =
            Array.map (\n -> { n | pos = n.pos - speed })
    in
    List.map
        (\l ->
            ( { l | notes = filterOut l.notes |> filterPress l.pressed |> moveNotes }
            , ( getScore l.pressed l.notes, getCombo l.pressed l.notes )
            )
        )
        ls
        |> List.unzip
        |> (\( filteredls, scoresAndCombos ) ->
                ( filteredls, List.unzip scoresAndCombos )
                    |> (\( retLane, ( retScore, retCombo ) ) ->
                            ( retLane
                            , ( List.foldl (+) 0 retScore, List.foldl addCombo (Continue 0) retCombo, List.filter ((==) Cut) retCombo |> List.length )
                            , pressedNotes ls )
                       )
           )


xOfLane : Lane -> Number
xOfLane l = 
    toFloat (2 * l.id - mentionedKeysLen + 1)
        / 2
        |> (*) panelInterval
    

lane : Lane -> List Shape
lane l =
    [ rectangle lightGrey laneWidth 400
    , square
        (if l.pressed then
            red

         else
            blue
        )
        panelSize
    , words white l.key
    ]
        ++ (l.notes
                |> Array.map
                    (\n -> notePanel |> List.map (moveY n.pos))
                |> Array.toList
                |> List.concat
           )
        |> List.map
            (move
                ( xOfLane l )
                panelHeight
            )


lanes : List Lane -> List Shape -> List Shape
lanes ls lst =
    List.map lane ls
        |> List.concat
        |> List.map (scale panelScale)
        |> (\shapes -> shapes ++ lst)
