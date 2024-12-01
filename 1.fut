import "utils"

def parse (s: string []) =
  let (get, ls) = lines.lines s
  let f l =
    let l = get l
    let (x, l) = span (== ' ') l
    let (_, y) = span (!= ' ') l
    in (atoi x, atoi y)
  in unzip (map f ls)

entry part1 (s: string []) =
  s
  |> parse
  |> sort (<=) *** sort (<=)
  |> uncurry (map2 (-))
  |> map i64.abs
  |> i64.sum

-- ==
-- entry: part1
-- script input { $loadbytes "inputs/1.input" }
-- output { 2086478i64 }

-- Inefficient brute force.
entry part2 (s: string []) =
  s
  |> parse
  |> (\(xs, ys) -> map (\x -> x * count (== x) ys) xs)
  |> i64.sum

-- ==
-- entry: part2
-- script input { $loadbytes "inputs/1.input" }
-- output { 24941624i64 }
