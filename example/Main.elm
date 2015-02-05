import Benchmark (benchmarkFmt, unify)

import Array
import List ((::))
import Signal (Signal, map)

import Color (white)
import Graphics.Collage (..)
import Graphics.Element (..)

import Html (toElement)

main : Signal Element
main =
  map
    (toElement 500 500)
    (benchmarkFmt 10000
      [ ("2d Array -> List Form", unify (\_ ->
          let
            grid = Array.repeat 720 (Array.repeat 480 white)
            lst = []
            _ = Array.indexedMap (\x col ->
                  Array.indexedMap (\y color ->
                    (square 1
                      |> filled color
                      |> move (-(toFloat 720 / 2) + toFloat x, (toFloat 480 / 2) - toFloat y))
                    :: lst)
                  col)
                grid
          in lst))
      ])
