-- Straightforward parallel brute force.

import "utils"

def safe (xs: []i32) =
  -- This could be done as a near-homomorphism, but the inputs are so
  -- small that it doesn't matter.
  let op (asc, desc, adj, prev) x =
    ( asc && x > prev
    , desc && x < prev
    , adj && i32.abs (prev - x) >= 1 && i32.abs (prev - x) <= 3
    , x
    )
  let (asc, desc, adj, _) = foldl op (true, true, true, xs[0]) xs[1:]
  let ok = (asc || desc) && adj
  in ok

entry part1 (xs: string []) =
  let (getl, ls) = lines.lines xs
  let f l =
    let l = getl l
    let (getw, ws) = words.words l
    in safe (map (\w -> atoi32 (getw w)) ws)
  in count32 f ls

-- ==
-- entry: part1
-- script input { $loadbytes "inputs/2.input" }
-- output { 639 }

entry part2 (xs: string []) =
  let (getl, ls) = lines.lines xs
  let f l =
    let l = getl l
    let (getw, ws) = words.words l
    let safe' i = safe (map (\w -> atoi32 (getw w)) (take i ws ++ drop (i + 1) ws))
    in any safe' (indices ws)
  in count32 f ls

-- ==
-- entry: part2
-- script input { $loadbytes "inputs/2.input" }
-- output { 674 }
