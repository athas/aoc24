import "utils"

def read_mul (s: string []) =
  if len s < 4 || !(and (map2 (==) (take 4 s) "mul("))
  then (0, 0)
  else let s = drop 4 s
       let (a, s) = span isnt_digit s
       in if len s == 0 || s[0] != ','
          then (0, 0)
          else let (b, s) = span isnt_digit (drop 1 s)
               in if len s == 0 || s[0] != ')'
                  then (0, 0)
                  else (atoi32 a, atoi32 b)

def read_dodont (s: string []) : opt (#do | #dont) =
  if len s >= 7 && and (map2 (==) (take 7 s) "don't()")
  then #some #dont
  else if len s >= 4 && and (map2 (==) (take 4 s) "do()")
  then #some #do
  else #none

def parse [n] (s: string [n]) =
  tabulate n (\i -> read_mul (drop i s))

entry part1 (s: string []) =
  parse s |> unzip |> uncurry (map2 (*)) |> i32.sum

def parse_dodonts [n] (s: string [n]) =
  let op x y =
    match (x, y)
    case (_, #some _) -> y
    case (#some _, _) -> x
    case (#none, #none) -> #none
  in tabulate n (\i -> if i == 0 then #some #do else read_dodont (drop i s))
     |> scan op #none

entry part2 (s: string []) =
  s
  |> parse &&& parse_dodonts
  |> uncurry zip
  |> map (\((a, b), m) ->
            match m
            case #some #do -> a * b
            case _ -> 0)
  |> i32.sum

-- ==
-- entry: part1
-- script input { $loadbytes "inputs/3.input" }
-- output { 188192787 }

-- ==
-- entry: part2
-- script input { $loadbytes "inputs/3.input" }
-- output { 113965544 }
