module Benchmark(benchmark) where

import Time (..)
import Signal (..)

benchmark : String -> Int -> (a -> b) -> Signal String
benchmark label ops fn = map (fmt label) (timer ops fn)

timer : Int -> (a -> b) -> Signal (Time, Time)
timer ops fn=
  map2 (,)
    (startTime)
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
startTime : Signal Time
startTime = constant () |> timestamps

timestamps : Signal a -> Signal Time
timestamps s = timestamp s ~> fst

(~>) : Signal a -> (a -> b) -> Signal b
(~>) = flip map
infixl 4 ~>
