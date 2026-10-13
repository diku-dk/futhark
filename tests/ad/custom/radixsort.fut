-- Custom derivative for radix sort.
-- ==
-- tags { autodiff }
-- entry: main_standard main_custom
-- input { [4f32,3f32,2f32,1f32] [0.1f32,0.2f32,0.3f32,0.4f32] }
-- output { [0.4f32, 0.3f32, 0.2f32, 0.1f32 ] }

-- ==
-- entry: main_custom_vec
-- input { [4f32,3f32,2f32,1f32] [[0.1f32,0.2f32,0.3f32,0.4f32],[0.5f32,0.6f32,0.7f32,0.8f32]] }
-- output { [[0.4f32, 0.3f32, 0.2f32, 0.1f32], [0.8f32, 0.7f32, 0.6f32, 0.5f32]] }

def radix_sort_step [n] 't (f: t -> u32) (xs: [n]t) (b: i32) : [n]t =
  let bits = map (\x -> (i32.u32 (f x >> u32.i32 b)) & 1) xs
  let bits_neg = map (1 -) bits
  let offs = reduce (+) 0 bits_neg
  let idxs0 = map2 (*) bits_neg (scan (+) 0 bits_neg)
  let idxs1 = map2 (*) bits (map (+ offs) (scan (+) 0 bits))
  let idxs2 = map2 (+) idxs0 idxs1
  let idxs = map (\x -> x - 1) idxs2
  let xs' = scatter (copy xs) (map i64.i32 idxs) xs
  in xs'

def radix_sort [n] 't (f: t -> u32) (xs: [n]t) : [n]t =
  loop xs for i < 32 do radix_sort_step f xs i

def differentiable_radix_sort [n] 't (f: t -> u32) (xs: [n]t) =
  (with_vjp (\xs ->
               unzip (radix_sort (f <-< (.0)) (zip xs (iota n))))
            (\(_, perm) (xs_adj, _) ->
               scatter (copy xs_adj) perm xs_adj)
            xs).0

entry main_standard = vjp (radix_sort f32.to_bits)
entry main_custom = vjp (differentiable_radix_sort f32.to_bits)
entry main_custom_vec = mjp (differentiable_radix_sort f32.to_bits)
