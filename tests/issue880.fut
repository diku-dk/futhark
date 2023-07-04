-- ==
-- error: Consuming.*"xs"

type t = {xs: [10]i32}

def f ({xs}: t) : t = {xs = xs}

def g (s: t) =
  let s = f s
  let s = loop s = f s for _i < 10 do f s
  in s

def main xs =
  let {xs} = g {xs}
  in xs with [0] = 0
