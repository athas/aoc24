import "utils"

def parse [l] (s: string [l]) =
  let m = count (== '\n') s
  let n = l / m
  in map init (unflatten (sized (m * n) s))

def get [n] [m] i j (xs: [n][m]char) =
  if i >= 0 && i < n && j >= 0 && j < m
  then xs[i, j]
  else '.'

def fetch n xs di dj i j =
  tabulate n (\l -> get (i + l * di) (j + l * dj) xs)

def xmass_at xs i j =
  count32 (\(di, dj) -> arreq (==) (fetch 4 xs di dj i j) "XMAS")
          [ (0, 1)
          , (0, -1)
          , (1, 0)
          , (-1, 0)
          , (1, 1)
          , (-1, 1)
          , (1, -1)
          , (-1, -1)
          ]

def xmasses [n] [m] (A: [n][m]char) =
  tabulate_2d n m (xmass_at A) |> flatten |> i32.sum

entry part1 s = parse s |> xmasses

def crossmass_at A i j =
  let a = fetch 3 A 1 1 (i - 1) (j - 1)
  let b = fetch 3 A (-1) 1 (i + 1) (j - 1)
  in i32.bool ((arreq (==) a "MAS"
                || arreq (==) a "SAM")
               && (arreq (==) b "MAS"
                   || arreq (==) b "SAM"))

def crossmasses [n] [m] (A: [n][m]char) =
  tabulate_2d n m (crossmass_at A) |> flatten |> i32.sum

entry part2 s = parse s |> crossmasses

-- ==
-- script input { $loadbytes "inputs/4.input" }
-- output { 2493 }
