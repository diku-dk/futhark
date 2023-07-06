-- Based on #1950.
-- ==
-- error: Causality check

type option 'a = #None | #Some a

def gen () : ?[n].[n]i32 =
  let (n,_) = (0,true)
  in replicate n 0i32

def ite b t f = if b then t() else f()

entry main b: option ([]i32) =
  ite b (\() -> #None) (\() -> #Some(gen ()))
