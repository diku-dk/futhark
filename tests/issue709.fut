-- ==
-- input { 0 } output { [[[0]]] }

def insert [n] 't (np1: i64) (x: t) (a: [n]t) (i: i64): [np1]t =
  let (b,c) = (take i a, drop i a)
  in b ++ [x] ++ c :> [np1]t

def list_insertions [n] 't (np1: i64) (x: t) (a: [n]t): [np1][np1]t =
  map (insert np1 x a) (0...(length a)) :> [np1][np1]t

def main (x: i32) = map (list_insertions 1 x) [[]]
