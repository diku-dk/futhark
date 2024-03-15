def part2Indices [n] 't (conds: [n]bool) : {bool | \res-> permutationOf res (0...n-1)} =
  let lol = if conds[0] then true else false
  let res = not (lol || false)
  in res
