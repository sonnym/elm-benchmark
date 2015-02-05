import Benchmark (benchmarkFmt, unify)

import Signal (Signal, map)

import Graphics.Element (Element)

import Graphics.Collage as C
import Graphics.Element as E

import Html (toElement)

main : Signal Element
main =
  map
    (toElement 500 500)
    (benchmarkFmt 10000
      [ ("identity", unify identity)
      , ("multiplication", unify (\_ -> 2 * 2))
      , ("E.empty |> C.toForm", unify (\_ -> E.empty |> C.toForm))
      ])
