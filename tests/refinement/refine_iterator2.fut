-- Want:
--   ∀i₆₁₇₂ ∈ iota n₆₁₂₇ .
--     | True => Σj₆₁₇₀∈[0, ..., i₆₁₇₂] ((xs₆₁₂₈)[j₆₁₇₀])
def sum_evil [n] (xs: [n]i64) : {[n]i64 | \res-> permutationOf res (0...6)} =
  let ys = scan (+) 0 xs
  let iota_n = iota n
  let zs = map (\i -> if i <= n then ys[i] else i) iota_n
  in zs
