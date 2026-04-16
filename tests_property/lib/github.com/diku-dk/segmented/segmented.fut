-- | Irregular segmented operations, like scans and reductions.

-- | Segmented scan. Given a binary associative operator ``op`` with
-- neutral element ``ne``, computes the inclusive prefix scan of the
-- segments of ``as`` specified by the ``flags`` array, where `true`
-- starts a segment and `false` continues a segment.
def segmented_scan [n] 't
                   (op: t -> t -> t)
                   (ne: t)
                   (flags: [n]bool)
                   (as: [n]t) : *[n]t =
  (unzip (scan (\(x_flag, x) (y_flag, y) ->
                  ( x_flag || y_flag
                  , if y_flag then y else x `op` y
                  ))
               (false, ne)
               (zip flags as))).1

-- | Segmented reduction. Given a binary associative operator ``op``
-- with neutral element ``ne``, computes the reduction of the segments
-- of ``as`` specified by the ``flags`` array, where `true` starts a
-- segment and `false` continues a segment.  One value is returned per
-- segment.
def segmented_reduce [n] 't
                     (op: t -> t -> t)
                     (ne: t)
                     (flags: [n]bool)
                     (as: [n]t) =
  segmented_scan op ne flags as
  |> zip (rotate 1 flags)
  |> filter (.0)
  |> map (.1)

-- | Replicated iota. Given a repetition array, the function returns
-- an array with each index (starting from 0) repeated according to
-- the repetition array. As an example, replicated_iota [2,3,1]
-- returns the array [0,0,1,1,1,2].
def replicated_iota [n] (reps: [n]i64) : []i64 =
  let offsets = scan (+) 0 reps
  let size = reduce_comm (+) 0 reps
  let tmp = scatter (replicate size 0) offsets (iota n |> map (+ 1))
  in scan i64.max i64.lowest tmp

-- | Segmented iota. Given a flags array, the function returns an
-- array of index sequences, each of which is reset according to the
-- flags array. As an examples, segmented_iota
-- [false,false,false,true,false,false,false] returns the array
-- [0,1,2,0,1,2,3].
def segmented_iota [n] (flags: [n]bool) : *[n]i64 =
  let iotas = segmented_scan (+) 0 flags (replicate n 1)
  in map (\x -> x - 1) iotas

-- | Replicated and segemented iota generated together
-- in a slighly more efficient way.
-- each segment in the segmented iota corresponds to a segment
-- in the replicated iota. As an example repl_segm_iota [2,3,1]
-- returns the arrays [0,0,1,1,1,2] and [0,1,0,1,2,0].
def repl_segm_iota [n] (reps: [n]i64) : (*[]i64, *[]i64) =
  let offsets = scan (+) 0 reps
  let size = reduce_comm (+) 0 reps
  let tmp = scatter (rep 0) offsets (iota n |> map (+ 1))
  let repl = scan i64.max i64.lowest tmp
  let segm = map2 (\i r -> i - if r == 0 then 0 else offsets[r - 1]) (iota size) repl
  in (repl, segm)

-- | Generic expansion function. The function expands a source array
-- into a target array given (1) a function that determines, for each
-- source element, how many target elements it expands to and (2) a
-- function that computes a particular target element based on a
-- source element and the target element number associated with the
-- source. As an example, the expression expand (\x->x) (*) [2,3,1]
-- returns the array [0,2,0,3,6,0].
def expand 'a 'b (sz: a -> i64) (get: a -> i64 -> b) (arr: []a) : *[]b =
  let szs = map sz arr
  let (idxs, iotas) = repl_segm_iota szs
  in map2 (\i j -> get arr[i] j) idxs iotas

-- | Expansion function equivalent to performing a segmented reduction
-- to the result of a general expansion with a flags vector expressing
-- the beginning of the expanded segments. The function makes use of
-- the intermediate flags vector generated as part of the expansion
-- and the `expand_reduce` function is therefore more efficient than
-- if a segmented reduction (with an appropriate flags vector) is
-- explicitly followed by a call to expand.
def expand_reduce 'a 'b
                  (sz: a -> i64)
                  (get: a -> i64 -> b)
                  (op: b -> b -> b)
                  (ne: b)
                  (arr: []a) : *[]b =
  let szs = map sz arr
  let idxs = replicated_iota szs
  let flags = map2 (!=) idxs (rotate (-1) idxs)
  let iotas = segmented_iota flags
  let vs = map2 (\i j -> get arr[i] j) idxs iotas
  in segmented_reduce op ne flags vs

-- | Expansion followed by an ''outer segmented reduce'' that ensures
-- that each element in the result array corresponds to expanding and
-- reducing the corresponding element in the source array.
def expand_outer_reduce 'a 'b [n]
                        (sz: a -> i64)
                        (get: a -> i64 -> b)
                        (op: b -> b -> b)
                        (ne: b)
                        (arr: [n]a) : *[n]b =
  let sz' x =
    let s = sz x
    in if s == 0 then 1 else s
  let get' x i = if sz x == 0 then ne else get x i
  in expand_reduce sz' get' op ne arr :> [n]b
