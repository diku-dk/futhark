-- n== 
-- property: prop_generator_nontermination 
entry gen_nonterminates (size: i64) (seed: u64) : i32 = 
    loop x = 0i32 
    while true do 1+1


#[nprop(gen(gen_nonterminates))] 
entry prop_generator_nontermination (x: i32) : bool = false