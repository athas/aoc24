-- Sadly sequential, but otherwise nice. I'm betting there is a way to
-- use speculative parallelism here.
--
-- Part 2 screams for a recursive solution, but, well, Futhark. Brute force.

import "utils"

def parse (s: string []) =
  let (get, ls) = lines.lines s
  let m = len (get ls[0])
  in map (\l -> take m (get l)) ls

def rotate (x: char) =
  match x
  case '^' -> '>'
  case '>' -> 'v'
  case 'v' -> '<'
  case '<' -> '^'
  case c -> c

def ahead (x: char) (i, j) : (i64, i64) =
  match x
  case '^' -> (i - 1, j)
  case '>' -> (i, j + 1)
  case 'v' -> (i + 1, j)
  case '<' -> (i, j - 1)
  case _ -> (i, j)

def empty (x: char) = x == '.' || x == 'X'

def find_pos [n] [m] (grid: [n][m]char) =
  tabulate_2d n m (\i j ->
                     if rotate (grid[i, j]) != grid[i, j] then (i, j) else (-1, -1))
  |> flatten
  |> unzip
  |> i64.maximum *** i64.maximum

def march grid =
  loop (grid, p) = (copy grid, find_pos grid)
  while in_bounds p grid do
    let p' = ahead grid[p.0, p.1] p
    in if !in_bounds p' grid
       then (grid with [p.0, p.1] = 'X', p')
       else if empty grid[p'.0, p'.1]
       then ( grid with [p'.0, p'.1] = grid[p.0, p.1] with [p.0, p.1] =
                'X'
            , p'
            )
       else ( grid with [p.0, p.1] = rotate grid[p.0, p.1]
            , p
            )

entry part1 (s: string []) =
  let [n][m] (grid: [n][m]char) = parse s
  let (grid, _) = march grid
  in flatten grid |> count32 (== 'X')

def max_steps : i32 = 10000

def loops grid =
  (== max_steps)
  <| (.2)
  <| loop (grid, p, i) = (copy grid, find_pos grid, 0)
  while in_bounds p grid && i < max_steps do
    let p' = ahead grid[p.0, p.1] p
    in if !in_bounds p' grid
       then (grid with [p.0, p.1] = 'X', p', i + 1)
       else if empty grid[p'.0, p'.1]
       then ( grid with [p'.0, p'.1] = grid[p.0, p.1] with [p.0, p.1] =
                'X'
            , p'
            , i + 1
            )
       else ( grid with [p.0, p.1] = rotate grid[p.0, p.1]
            , p
            , i + 1
            )

def block (grid: [][]char) (i: i64, j: i64) = copy grid with [i, j] = '#'

entry part2 (s: string []) =
  let [n][m] (grid: [n][m]char) = parse s
  let (grid', _) = march grid
  let positions =
    tabulate_2d n m (\i j -> (i, j)) |> flatten |> filter (\(i, j) -> grid'[i, j] == 'X')
  in count32 (loops <-< block grid) positions

-- ==
-- entry: part1
-- script input { $loadbytes "inputs/6.input" }
-- output { 4776 }

-- ==
-- entry: part2
-- script input { $loadbytes "inputs/6.input" }
-- output { 1586 }
