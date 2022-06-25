type sum = #foo | #bar
type t0 [n] = ([n]u32,f16,sum)
type t1 [n] = (t0 [n],[3]f32)

entry sum_const : sum = #foo

entry mk_t0 x y (s: sum) : t0 [] = (x,y,s)

entry mk_t1 (x: t0 []) : t1 [] = (x, [1,2,3])

entry main0 [n] (p: *t1 [n]): t1 [] = p

entry main1 [n] (y: t0 [n]) : t0 [] = y
