-- This one is ass. Too irregular. Still some parallelism.
--
-- Revealed a compiler bug:
-- https://github.com/diku-dk/futhark/issues/2197

import "utils"

def testinput =
  "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47\n"

entry part1 (s: string []) =
  let (get, ls) = lines.lines s
  -- First find the blank line.
  let blank l = null (get l)
  let pair l = (atoi32 (get l)[0:2], atoi32 (get l)[3:5])
  let (constraints, updates) = span blank ls |> (map pair *** tail)
  let check_constraint pages (x, y) =
    let xi = idxof (== x) pages
    let yi = idxof (== y) pages
    in xi == len pages || xi <= yi
  in map (\u ->
            let (getw, ws) = splits.splits (== ',') (get u)
            let pages = map (\w -> atoi32 (getw w)) ws
            in if all (check_constraint pages) constraints
               then pages[length pages / 2]
               else 0)
         updates
     |> i32.sum

-- ==
-- entry: part1
-- script input { $loadbytes "inputs/5.input" }
-- output { 5208 }

entry part2 (s: string []) =
  let (get, ls) = lines.lines s
  -- First find the blank line.
  let blank l = null (get l)
  let pair l = (atoi32 (get l)[0:2], atoi32 (get l)[3:5])
  let (constraints, updates) = span blank ls |> (map pair *** tail)
  let fixup (pages: *[]i32) (x, y) =
    let xi = idxof (== x) pages
    let yi = idxof (== y) pages
    in if xi == len pages || yi == len pages || xi <= yi
       then (pages, false)
       else (pages with [xi] = y with [yi] = x, true)
  in map (\u ->
            let (getw, ws) = splits.splits (== ',') (get u)
            let pages_orig = map (\w -> atoi32 (getw w)) ws
            let (pages, _) =
              loop (pages, i) = (copy pages_orig, 0)
              while i < length constraints do
                let (pages, fixed) = fixup pages constraints[i]
                in (pages, if fixed then 0 else i + 1)
            in if arreq (==) pages pages_orig
               then 0
               else pages[length pages / 2])
         updates
     |> i32.sum

-- ==
-- entry: part2
-- script input { $loadbytes "inputs/5.input" }
-- output { 6732 }
