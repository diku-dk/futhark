-- Is meant to to test nonterminationo of generator, but it is not
-- part of the standard suite, because it sometimes seems to hang
-- indefinitely. Should be added later.

-- ==
-- property: prop_shrinker_nontermination

def diverge_u64 (start: u64) : u64 =
  loop i = start
  while i != i + 1u64 do
    i + 1u64

entry gen_failing (size: i64) (seed: u64) : i32 =
  -1i32

entry shrink_nonterminates (x: i32) (random: u64) : i32 =
  x + i32.u64 (diverge_u64 (random))

#[prop(gen(gen_failing), shrink(shrink_nonterminates))]
entry prop_shrinker_nontermination (x: i32) : bool =
  x >= 0i32