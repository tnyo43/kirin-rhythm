module Game exposing (..)

import Lane exposing (mentionedKeysLen)
import Playground exposing (..)

type alias Data =
    { bpm : Int
    , unit : NoteUnit
    , melody : Melody
    , elapsed : Int
    , clock : Number
    }

initData : Data
initData =
    { bpm = 128
    , unit = N4
    , melody = List.repeat mentionedKeysLen "fff"
    , elapsed = 0
    , clock = 0
    }

type NoteUnit
    = N4
    | N8
    | N16


type alias Melody = List String

isNoteAdd : Melody -> Int -> List Bool
isNoteAdd melody elapsed =
    List.map
        (\m ->
            True
        )
        melody

clock : Time -> Data -> ( Data, List Bool )
clock time data =
    let
        currentClock =
            spin 10 time

        tik =
            data.clock > currentClock
    in
    ( { data | clock = currentClock }
    , if tik then
        isNoteAdd data.melody data.elapsed 
      else
        List.repeat (List.length data.melody) False
    )