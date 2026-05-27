-- n==
-- property: prop_property_nontermination

def diverge_u64 (start: u64) : u64 =
  loop i = start
  while i != i + 1u64 do
    i + 1u64

entry gen_i32 (size: i64) (seed: u64) : i32 =
  0i32

#[nprop(gen(gen_i32))]
entry prop_property_nontermination (x: i32) : bool =
  diverge_u64 (u64.i32 x) == 0u64