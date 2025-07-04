-- ==
-- entry: main
-- compiled input {[0f32, 0f32, 0f32]} auto output
-- compiled random input {[10]f32} auto output

def map_func [n] (c: *[n]f32) : *[n]f32 =
  map (+ 1) c

entry main [n] (c: [n]f32) : [n]f32 =
  let acc_init = replicate n 0.0f32
  in loop acc = acc_init
     for _i < n do
       let c_copy = copy c
       let res = #[noinline] map_func c_copy
       in res with [0] = acc[0] + 1
