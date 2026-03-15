type real = f32
def i64fromReal = i64.f32

def partition2Inds [n] 
        (p: real -> bool)
        (xs:  [n]real) : (i64, *[n]i64) = 
  let cs = map (\x -> p x) xs
  let flagsT = map (\c -> if c then 1 else 0) cs
  let flagsF = map (\b -> 1 - b) flagsT
  let indicesT = scan (+) 0 flagsT
  let num_true = if n > 0 then indicesT[n-1] else 0
  let tmp      = scan (+) 0 flagsF
  let indicesF = map (\t -> t + num_true) tmp
  let indices  = map3 (\c t f -> if c then t-1 else f-1)
                    cs indicesT indicesF
  in  (num_true, indices)
--  let ys = scatter dst indices xs
--  in (num_true, ys)

def scatterDynChecked [n][m]
        (dst: *[n]real) (is: [m]i64) 
        (vs: [m]real) : *[n]real = 
  let mininds = hist (i64.min) i64.highest n is (iota m)
  let success =
        map2 ( \ind i -> mininds[ind] == i ) is (iota m)
        |> reduce (&&) true
  in assert success (scatter dst is vs)
 
def isOdd (x: real) : bool =
  0 == ((i64fromReal x) & 1)

-- Primes: Flat-Parallel Version
-- == entry: staticWithOpt staticChecked dynamicChecked
--
-- compiled random input {  [50000000]f32   [50000000]f32}
-- compiled random input { [100000000]f32  [100000000]f32}
-- compiled random input { [200000000]f32  [200000000]f32}

entry staticWithOpt [n] (dst: *[n]real) (xs: [n]real) : (i64, *[n]real) =
  -- let zeros = replicate n 0
  let (split,is) = partition2Inds isOdd xs
  let res = scatter dst is xs
  in  (split, res)
  
entry staticChecked [n] (_d: [n]real) (xs: [n]real) : (i64, [n]real) =
  let zeros = replicate n 0
  let (split,is) = partition2Inds isOdd xs
  let res = scatter zeros is xs
  in  (split, res)

entry dynamicChecked[n]  (_d: [n]real) (xs: [n]real) : (i64, [n]real) =
  let zeros = replicate n 0
  let (split,is) = partition2Inds isOdd xs
  let res = scatterDynChecked zeros is xs
  in  (split, res)
  


