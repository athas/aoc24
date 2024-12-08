-- Lovely problem. Lovely and even somewhat efficient parallel
-- implementation. Still brute force, naturally.

import "utils"

def parse (s: string []) =
  let (get, ls) = lines.lines s
  let m = length (get ls[0])
  in map (\l -> sized m (get l)) ls

def antennas (s: [][]char) : [](char, (i64, i64)) =
  imap (\i r -> imap (\j c -> if c != '.' then (c, (i, j)) else (c, (-1, -1))) r) s
  |> flatten
  |> filter ((.1) >-> (!= (-1, -1)))

def induces_at grid (i, j) (a: char, (ai, aj)) =
  let i' = ai + (ai - i)
  let j' = aj + (aj - j)
  in (i, j) != (ai, aj) && in_bounds (i', j') grid && a == grid[i', j']

entry part1 (s: string []) =
  let [n][m] (grid: [n][m]char) = parse s
  let as = antennas grid
  let is_antinode i j = any (induces_at grid (i, j)) as
  in tabulate_2d n m is_antinode |> flatten |> count id

-- This looks a bit clumsy.
def resonant_induces_at grid (i, j) (a: char, (ai, aj)) =
  let di = ai - i
  let dj = aj - j
  in (di != 0 || dj != 0)
     && let d = i64.abs (gcd di dj)
        let di = di / d
        let dj = dj / d
        let (_, _, count) =
          loop (ai, aj, count) = (i, j, i32.bool (a == grid[i, j]))
          while in_bounds (ai, aj) grid && count < 2 do
            ( ai + di
            , aj + dj
            , count + i32.bool (a == grid[ai, aj])
            )
        in count == 2

entry part2 (s: string []) =
  let [n][m] (grid: [n][m]char) = parse s
  let as = antennas grid
  let is_antinode i j = any (resonant_induces_at grid (i, j)) as
  in tabulate_2d n m is_antinode |> flatten |> count id
