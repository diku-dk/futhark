-- n==
-- property: prop_generator_nontermination

def diverge_u64 (start: u64) : u64 =
  loop i = start
  while i != i + 1u64 do
    i + 1u64

entry gen_nonterminates2 (size: i64) (seed: u64) : i32 =
  i32.u64 (diverge_u64 seed)

#[nprop(gen(gen_nonterminates2))]
entry prop_generator_nontermination2 (x: i32) : bool =
  true



-- == 
-- property: prop_generator_nontermination 
entry gen_nonterminates (size: i64) (seed: u64) : i32 = 
    loop x = 0i32 
    while true do 1+1


#[prop(gen(gen_nonterminates))] 
entry prop_generator_nontermination (x: i32) : bool = false