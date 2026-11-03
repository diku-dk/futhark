-- A port of the quick-select NESL implementation. As quick-sort, this
-- algorithm uses a divide-and-conquerer approach, but it needs only
-- recurse on one of the partitions as it will know in which one the
-- looked-for value resides.
--
-- Oh, and it cannot handle non-meaningful inputs.
--
-- ==
-- tags { no_csharp }
-- input { [1] 0i64 } output { 1 }
-- input { [4, -8, 2, 2, 0, 0, 5, 9, -6, 2] 7i64 } output { 4 }

def quickselect [n] (s: [n]i32) (k: i64) : i32 =
  let (_, s) =
    loop (k, s) while length s > 1 do
      let pivot = s[length s / 2]
      let (lt, gt, _) = partition2 (< pivot) (> pivot) s
      in if k < length lt
         then (k, lt)
         else if k >= length s - length gt
         then (k - (length s - length gt), gt)
         else (0, [pivot])
  in s[0]

def main (s: []i32) (k: i64) : i32 = quickselect s k
