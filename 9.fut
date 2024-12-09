-- Shamefully inefficient, but I wasn't very motivated today.

import "lib/github.com/diku-dk/segmented/segmented"
import "utils"

def checksum =
  imap (\i x -> if x == -1 then 0 else i * x)
  >-> i64.sum

def prev_used disk j =
  loop j while disk[j] == -1i64 do j - 1i64

def build_disk s =
  expand (.0)
         (\(_, i) _ -> if i % 2 == 0 then i / 2 else -1)
         (zip (map dtoi s) (indices s))

entry part1 (s: string []) =
  let disk = build_disk s
  let (disk, _, _) =
    loop (disk, i, j) = (disk, 0, prev_used disk (length disk - 1))
    while i < j do
      if disk[i] >= 0
      then (disk, i + 1, j)
      else let disk' = disk with [i] = disk[j] with [j] = -1
           in ( disk'
              , i + 1
              , prev_used disk' j
              )
  in checksum disk
