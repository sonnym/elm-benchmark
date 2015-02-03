module Benchmark(benchmark) where

import List
import Time (..)
import Signal (..)

import Signal.Extra ((~>))

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

fmt : String -> (Time, Time) -> String
fmt label result = label ++ ": " ++ (toString result)

-- "borrowed" from Apanatshka/elm-signal-extra
timestamps : Signal a -> Signal Time
timestamps s = timestamp s ~> fst

initSignal : Signal a -> Signal a
initSignal s = sampleOn (constant ()) s
