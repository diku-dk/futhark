-- ==

def segmented_scan [n] 't
                   (op: t -> t -> t)
                   (ne: t)
                   (flags: [n]bool)
                   (as: [n]t) : [n]t =
  (unzip (scan (\(x_flag, x) (y_flag, y) ->
                  ( x_flag || y_flag
                  , if y_flag then y else x `op` y
                  ))
               (false, ne)
               (zip flags as))).1

def replicated_iota [n] (reps: [n]i64) : []i64 =
  let s1 = scan (+) 0 reps
  let s2 =
    map2 (\i x -> if i == 0 then 0 else x)
         (iota n)
         (rotate (-1) s1)
  let tmp = reduce_by_index (replicate (reduce (+) 0 reps) 0) i64.max 0 s2 (iota n)
  let flags = map (> 0) tmp
  in segmented_scan (+) 0 flags tmp

def segmented_iota [n] (flags: [n]bool) : [n]i64 =
  let iotas = segmented_scan (+) 0 flags (replicate n 1)
  in map (\x -> x - 1) iotas

def expand 'a 'b (sz: a -> i64) (get: a -> i64 -> b) (arr: []a) : []b =
  let szs = map sz arr
  let idxs = replicated_iota szs
  let iotas = segmented_iota (map2 (!=) idxs (rotate (-1) idxs))
  in map2 (\i j -> get arr[i] j) idxs iotas

def sub xs (i: i64) = xs[i]

def flatMap 'a 'b [m] (n: i64) (f: a -> [n]b) (xs: [m]a) : *[]b =
  flatten (map f xs)

def sized 't n (xs: []t) = xs :> [n]t

entry listmults2 =
  let xss = [[1, 2, 3], [2, 3, 4]]
  let yss = [[4, 5, 6], [5, 6, 7]]
  in map (\(xs, ys, x, y) -> x * y)
         (expand (\(xs, ys, x) -> length ys)
                 (\(xs, ys, x) -> \y -> (xs, ys, x, sub ys y))
                 (expand (\(xs, ys) -> length xs)
                         (\(xs, ys) -> \x -> (xs, ys, sub xs x))
                         (flatMap (length yss)
                                  (\xs ->
                                     sized (length yss)
                                           (map (\ys -> (xs, ys)) yss))
                                  xss)))
