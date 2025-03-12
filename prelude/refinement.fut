import "soacs"
import "array"

def and (_: []bool) : bool = ???

def InjectiveRCD 't (_: []t) (_ : (i64, i64)) : bool = ???

def BijectiveRCD 't (_: []t) (_ : (i64, i64)) (_ : (i64, i64)) : bool = ???

def sum [n] (xs: [n]i64) =
  if n > 0 then (scan (+) 0 xs)[n-1] else 0

def to_i64 (c: bool): i64 = if c then 1 else 0

def all (p: i64 -> bool) (n: i64) =
  and (map (\i -> p i) (iota n))

def FiltPartInv [n] (_X : [n]i64) (_filt: i64 -> bool) (_part: i64 -> bool) =
  ???
  -- let filt_arr = map (\i -> filt i) (iota n)
  -- let m = sum (map (\i -> to_i64 (filt i)) (iota n))
  -- let step1 = bijectiveRCD (0, m-1) (0, m-1) X
  -- let step2 = and (map2 (\f x -> if f then true else x < 0 || x >= m) filt_arr X)
  -- let step3 = true -- needs i < j
  -- let step4 = true -- needs i < j
  -- let step5 = all (\i -> if part i then X[i] < s else X[i] >= s) m
  -- in step1 && step2 && step5
