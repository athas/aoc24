-- Lovely parallel, although brute force. A recursive implementation
-- could do a bit more pruning.
--
-- Part 2 totally fucked me over, however, as I had to give up on
-- using a nice bitmask for the possible operator insertion points.
-- Bloody elephants.

import "utils"

def max_nums : i64 = 12

def testinput = "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20\n"

def parse (s: string []) : [](i64, [max_nums]i64) =
  let (get, ls) = lines.lines s
  let on_line l =
    let (getw, ws) = words.words (get l)
    in ( atoi (getw ws[0])
       , tabulate max_nums
                  (\i ->
                     if i + 1 < length ws then atoi (getw ws[i + 1]) else 0)
       )
  in map on_line ls

-- Encode an operator insertion with a bitmask with bit 0 denoting
-- addition and bit 1 denoting multiplication.

def eval (xs: [max_nums]i64) (mask: u32) =
  (.0)
  <| loop (res, i) = (xs[0], 1)
  while i < i32.i64 max_nums && xs[i] != 0 do
    ( match u32.get_bit (i - 1) mask
      case 0 -> res + xs[i]
      case _ -> res * xs[i]
    , i + 1
    )

entry part1 (s: string []) =
  let eqs = parse s
  let solvable (lhs, rhs) =
    i64.bool (tabulate (1 << max_nums) (\i -> eval rhs (u32.i64 i))
              |> any (== lhs))
    * lhs
  in map solvable eqs |> i64.sum

-- ==
-- entry: part1
-- script input { $loadbytes "inputs/7.input" }
-- output { 4364915411363i64 }

def decdigits x = i64.f64 (f64.log10 (f64.i64 x) + 1)

def eval_ternary (xs: [max_nums]i64) (mask: u32) =
  (.0)
  <| loop (res, i, mask) = (xs[0], 1, mask)
  while i < i32.i64 max_nums && xs[i] != 0 do
    ( match mask % 3
      case 0 -> res + xs[i]
      case 1 -> res * xs[i]
      case _ -> res * 10 ** decdigits xs[i] + xs[i]
    , i + 1
    , mask / 3
    )

entry part2 (s: string []) =
  let eqs = parse s
  let solvable (lhs, rhs) =
    let k = index_of_first (== 0) rhs
    in (.0)
       <| loop (_, i) = (0, 0)
       while i < 3 ** k do
         if eval_ternary rhs (u32.i64 i) == lhs
         then (lhs, 3 ** max_nums)
         else (0, i + 1)
  in map solvable eqs |> i64.sum

-- ==
-- entry: part2
-- script input { $loadbytes "inputs/7.input" }
-- output { 38322057216320i64 }
