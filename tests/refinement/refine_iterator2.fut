-- This should _not_ touch the sum. Want:
--   ∀i₆₁₇₂ ∈ iota n₆₁₂₇ .
--     | i₆₁₇₂ <= n₆₁₂₇ => Σj₆₁₇₀∈[0, ..., i₆₁₇₂] ((xs₆₁₂₈)[j₆₁₇₀])
--     | ¬(i₆₁₇₂ <= n₆₁₂₇) => i₆₁₇₂
def sum_evil [n] (xs: [n]i64) : {[n]i64 | \res-> permutationOf res (0...6)} =
  let ys = scan (+) 0 xs
  let iota_n = iota n
  let zs = map (\i -> if i <= n then ys[i] else i) iota_n
  in zs
