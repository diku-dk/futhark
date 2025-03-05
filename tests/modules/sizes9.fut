-- From #2222
-- ==
-- input {}

module type QCircuit = {
  type Circuit [n]
  val create : (n: i64) -> Circuit [n]
  val H [n] : i64 -> *Circuit [n] -> *Circuit [n]
}

module i8Circuit : QCircuit = {
  type Circuit [n] = [n][2 * n]i8

  def create (n) : *[n][2 * n]i8 =
    map (\_ -> replicate (2 * n) 0i8) (iota n)

  def H [n]
        (a: i64)
        (x: *[n][2 * n]i8) : *Circuit [n] =
    let x[a] = replicate (2 * n) 1i8
    in x
}

def main =
  let circ = i8Circuit.create 5
  let circ = i8Circuit.H 2 circ
  in circ
