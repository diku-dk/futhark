def global_array : []f64 =
  [ 0.0
  , 0.84147096
  , 0.9092974
  , 0.14112
  , -0.7568025
  , -0.9589243
  , -0.2794155
  , 0.6569866
  , 0.98935825
  , 0.4121185
  ]

def f (i: i64) =
  let x = #[unsafe] global_array[i % length global_array]
  in x * x * x * x * x * x * x * x * x * x * x * x

-- ==
-- entry: main
-- input { [0i64, 5i64, 6i64] }
-- output { [0.0, 0.6045216932648491, 2.264675222032749e-7] }

entry main (is: []i64) =
  map f is

entry main2 (is: []i64) =
  map f is
