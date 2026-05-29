-- Is meant to to test nonterminationo of generator, but it is not
-- part of the standard suite, because it sometimes seems to hang
-- indefinitely. Should be added later.

-- == 
-- property: prop_generator_nontermination 
entry gen_nonterminates (size: i64) (seed: u64) : i32 = 
    loop x = 0i32 
    while true do 1+1


#[prop(gen(gen_nonterminates))] 
entry prop_generator_nontermination (x: i32) : bool = false
