-- Based on #1741
-- ==
-- error: type t = i64

module Op = (\(X: {type t = i64 val x : t}) -> {def x = X.x})
            : (X: {type t val x: t}) -> {val x : X.t}

module L2 = Op {
  type t = bool
  def x : t = true
}

def main = L2.x
