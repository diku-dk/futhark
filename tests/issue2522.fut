def all_unique [n] (size: i64) (is: [n]i64) : bool =
  scatter (replicate size 0) is (rep 1)
  |> i64.sum
  |> (== n)

-- ==
-- tags { autodiff }
-- entry: main
-- input { [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]] }
-- output { [[1.0, 1.0, 1.0], [1.0, 1.0, 1.0]] }
-- input { [[1.0, 2.0, 3.0], [7.0, 8.0, 9.0]] }
-- output {  [[1.0, 1.0, 1.0], [0.0, 0.0, 0.0]] }

def partition' [n] 'a
               (p: a -> bool)
               (as: [n]a) : ?[k].([k]a, [n - k]a) =
  let to_index0 f (o0, _o1) = if f then o0 - 1 else -1
  let to_index1 f (_o0, o1) = if f then n - o1 else -1
  let add2 (a0, b0) (a1, b1) = (a0 + a1, b0 + b1)
  let t_flags = map p as
  let rev_as = reverse as
  let f_flags = map p rev_as |> map (\x -> !x)
  let flags =
    map2 (\x y ->
            ( i64.bool x
            , i64.bool y
            ))
         t_flags
         f_flags
  let offsets = scan add2 (0, 0) flags
  let idxs0 = map2 to_index0 t_flags offsets
  let idxs1 = map2 to_index1 f_flags offsets
  let is = idxs0 ++ idxs1
  let res = scatter (copy [as][0]) (assert (not (all_unique (2 * n) is)) is) (as ++ rev_as)
  let count =
    scatter [0]
            (map (\j -> if j == n - 1 then 0 else -1) (0..1..<n))
            (map (.0) offsets)
  in (res[0:count[0]], res[count[0]:n])

def f [n] (xs: [n][3]f64) =
  let p x = if x[0] < 5 then true else false
  let (res, _sizes) = partition' p xs
  in res |> flatten |> f64.sum

entry main xs =
  vjp f xs 1.0
