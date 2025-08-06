-- A tricky test of type-level programming.
-- ==
-- input { [1,2,3] [4,5,6] [7,8,9] }
-- output { [1, 2, 3, 4, 5, 6, 7, 8, 9]
--          [4, 5, 6, 1, 2, 3, 7, 8, 9]
--        }

module meta
  : {
      type eq [n] [m]

      val coerce [n] [m] 't : eq [n] [m] -> [n]t -> [m]t
      val coerce_inner [n] [m] 't [k] : eq [n] [m] -> [k][n]t -> [k][m]t

      val refl [n] : eq [n] [n]
      val comm [n] [m] : eq [n] [m] -> eq [m] [n]
      val trans [n] [m] [k] : eq [n] [m] -> eq [m] [k] -> eq [n] [k]

      val plus_comm [a] [b] : eq [a + b] [b + a]
      val plus_assoc [a] [b] [c] : eq [(a + b) + c] [a + (b + c)]
      val plus_lhs [a] [b] [c] : eq [a] [b] -> eq [a + c] [b + c]
      val plus_rhs [a] [b] [c] : eq [c] [b] -> eq [a + c] [a + b]

      val mult_comm [a] [b] : eq [a * b] [b * a]
      val mult_assoc [a] [b] [c] : eq [(a * b) * c] [a * (b * c)]
      val mult_lhs [a] [b] [c] : eq [a] [b] -> eq [a + c] [b + c]
      val mult_rhs [a] [b] [c] : eq [c] [b] -> eq [a + c] [a + b]
    } = {
  type eq [n] [m] = [0][n][m]()

  def coerce [n] [m] 't (_: eq [n] [m]) (a: [n]t) = a :> [m]t
  def coerce_inner [n] [m] 't [k] (_: eq [n] [m]) (a: [k][n]t) = a :> [k][m]t

  def refl = []
  def comm _ = []
  def trans _ _ = []

  def plus_comm = []
  def plus_assoc = []
  def plus_lhs _ = []
  def plus_rhs _ = []

  def mult_comm = []
  def mult_assoc = []
  def mult_lhs _ = []
  def mult_rhs _ = []
}

def main [n] [m] [l] (xs: [n]i32) (ys: [m]i32) (zs: [l]i32) =
  let proof: meta.eq [m + (n + l)] [(n + m) + l] =
    meta.comm meta.plus_assoc `meta.trans` meta.plus_lhs meta.plus_comm
  in zip ((xs ++ ys) ++ zs) (meta.coerce proof (ys ++ (xs ++ zs)))
     |> unzip
