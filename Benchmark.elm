module Benchmark(benchmark, benchmarkFmt) where

import List
import List ((::))
import Time (..)
import Signal (..)

import Html (..)
import Signal.Extra ((~>), mapMany, zip)

benchmarkFmt : Int -> List (String, (a -> b)) -> Signal Html
benchmarkFmt ops labeledFns =
  let
    labels = List.map (fst >> constant) labeledFns
    results = benchmark ops (List.map snd labeledFns)
  in
    mapMany
      (\rows -> table [] [tblHead, tbody [] rows])
      (List.map2 (map2 (tblRow ops)) labels results)

benchmark : Int -> List (a -> b) -> List (Signal (Time,Time))
benchmark ops fns = List.map (timer ops) fns

timer : Int -> (a -> b) -> Signal (Time, Time)
timer ops fn =
  map2 (,)
    (initSignal (every millisecond))
    (timestamps (timestamp (keepIf (identity) False (runFn ops fn))))

runFn : Int -> (a -> b) -> Signal Bool
runFn ops fn =
  map
    (\_ ->
      let
        _ = List.repeat ops fn
      in
        True)
    (constant ())

tblHead : Html
tblHead =
  thead []
    [ tr []
      [ th [] [text "Label"]
      , th [] [text "Start"]
      , th [] [text "Finish"]
      , th [] [text "Average"]
      ]
    ]

tblRow : Int -> String -> (Time, Time) -> Html
tblRow ops label (start, finish) =
  let
    avg = (finish - start) / (toFloat ops)
  in
    tr []
      [ td [] [text label]
      , td [] [text (toString start)]
      , td [] [text (toString finish)]
      , td [] [text ((toString avg) ++ "ms")]
      ]

-- "borrowed" from Apanatshka/elm-signal-extra
timestamps : Signal a -> Signal Time
timestamps s = timestamp s ~> fst

initSignal : Signal a -> Signal a
initSignal s = sampleOn (constant ()) s
