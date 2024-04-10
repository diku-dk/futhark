-- Want:
-- ∀i₆₂₁₃ ∈ iota n₆₁₆₃ .
--     | i₆₂₁₃ == 3 => Σj₆₂₁₁∈[0, ..., 3] ((xs₆₁₆₄)[j₆₂₁₁])
--     | ¬(i₆₂₁₃ == 3) => i₆₂₁₃
def sum_literal_bound [n] (xs: [n]i64) : {[n]i64 | \res-> permutationOf res (0...6)} =
  let ys = scan (+) 0 xs
  let iota_n = iota n
  let zs = map (\i -> if i == 3 then ys[i] else i) iota_n
  in zs
