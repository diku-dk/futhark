-- Without refinement, get
--   ∀i₆₁₆₂ ∈ iota n₆₀₉₀ .
--       | i₆₁₆₂ == 0 => Σj₆₁₆₀∈[0, ..., i₆₁₆₂] ((xs₆₀₉₁)[j₆₁₆₀])
--       | ¬(i₆₁₆₂ == 0) => 1337
-- which should be refined to
--   ∀i₆₁₆₂ ∈ iota n₆₀₉₀ .
--       | i₆₁₆₂ == 0 => (xs₆₀₉₁)[0]
--       | ¬(i₆₁₆₂ == 0) => 1337
-- by using the bound on i in the first case.
def sum_one_term [n] (xs: [n]i64) : {[n]i64 | \res-> permutationOf res (0...6)} =
  let ys = scan (+) 0 xs
  let iota_n = iota n
  let zs = map (\i -> if i == 0 then ys[i] else i) iota_n
  in zs

-- This should _not_ touch the sum. Want:
--   ∀i₆₁₇₂ ∈ iota n₆₁₂₇ .
--     | i₆₁₇₂ <= n₆₁₂₇ => Σj₆₁₇₀∈[0, ..., i₆₁₇₂] ((xs₆₁₂₈)[j₆₁₇₀])
--     | ¬(i₆₁₇₂ <= n₆₁₂₇) => i₆₁₇₂
def sum_evil [n] (xs: [n]i64) : {[n]i64 | \res-> permutationOf res (0...6)} =
  let ys = scan (+) 0 xs
  let iota_n = iota n
  let zs = map (\i -> if i <= n then ys[i] else i) iota_n
  in zs

-- This should _not_ touch the sum. Want:
--   ∀i₆₁₇₂ ∈ iota n₆₁₂₇ .
--     | i₆₁₇₂ < 0 => Σj₆₁₇₀∈[0, ..., i₆₁₇₂] ((xs₆₁₂₈)[j₆₁₇₀])
--     | ¬(i₆₁₇₂ < 0) => i₆₁₇₂
-- TODO should a neutral element be inserted here?
def sum_empty [n] (xs: [n]i64) : {[n]i64 | \res-> permutationOf res (0...6)} =
  let ys = scan (+) 0 xs
  let iota_n = iota n
  let zs = map (\i -> if i < 0 then ys[i] else i) iota_n
  in zs

-- Want:
-- ∀i₆₂₁₃ ∈ iota n₆₁₆₃ .
--     | i₆₂₁₃ == 3 => Σj₆₂₁₁∈[0, ..., 3] ((xs₆₁₆₄)[j₆₂₁₁])
--     | ¬(i₆₂₁₃ == 3) => i₆₂₁₃
def sum_literal_bound [n] (xs: [n]i64) : {[n]i64 | \res-> permutationOf res (0...6)} =
  let ys = scan (+) 0 xs
  let iota_n = iota n
  let zs = map (\i -> if i == 3 then ys[i] else i) iota_n
  in zs

-- TODO support this.
-- def scan_slice (xs: [10]i64) : {[10-3]i64 | \res-> permutationOf res (0...6)} =
--   let ys = scan (+) 0 xs[3:]
--   in ys
