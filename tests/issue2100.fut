entry main (n: i64): () =
  let arr = reduce_by_index
            (replicate n ())
            (\() () -> ())
            ()
            (replicate n 0)
            (replicate n ())
  in arr[0]
