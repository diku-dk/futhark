-- ==
-- property: prop_pretty_printer_nontermination

def diverge_u64 (start: u64) : u64 =
  loop i = start
  while i != i + 1u64 do
    i + 1u64

entry gen_failing (size: i64) (seed: u64) : i32 =
  1i32

entry shrink_to_passing (x: i32) (random: u64) : i32 =
  0i32

entry pprint_nonterminates (x: i32) : []u8 =
  let y = diverge_u64 (u64.i32 x)
  in if y == 0u64 then "zero" else "unreachable"

#[prop(gen(gen_failing), shrink(shrink_to_passing), pprint(pprint_nonterminates))]
entry prop_pretty_printer_nontermination (x: i32) : bool =
  x <= 0i32