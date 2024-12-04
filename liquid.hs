import Data.Int

-- Futhark prelude.

{-@ scan :: (b -> a -> b) -> b -> {xs : [a] | True } -> {ys : [b] | len ys = len xs} @-}
scan op ne xs = tail $ scanl op ne xs

{-@ map1 :: (a -> b) -> {xs : [a] | True } -> {ys : [b] | len ys = len xs} @-}
map1 = map

{-@ zip2 :: {xs : [a] | True } -> {ys : [b] | len ys = len xs} -> {zs : [(a,b)] | len zs = len xs} @-}
zip2 = zip

{-@ map2 :: (a -> b -> c) -> {as : [a] | True } -> {bs : [b] | len bs = len as } -> {cs : [c] | len cs = len as } @-}
map2 f xs ys = map1 (uncurry f) $ zip2 xs ys

{-@ map3 :: (a -> b -> c -> d) -> {as : [a] | True } -> {bs : [b] | len bs = len as } -> {cs : [c] | len cs = len as } -> {ds : [d] | len ds = len as} @-}
map3 f xs ys zs = map1 (\((x,y),z) -> f x y z) $ zip2 (zip2 xs ys) zs


-- Programs.

{-@ type Nat = {v:Int | 0 <= v} @-}

{-@ simple :: {cs : [Bool] | True } -> {is : [Nat] | len is = len cs } @-}
simple :: [Bool] -> [Int]
simple cs =
  let is = map (\c -> if c then 1 else 0) cs
  in map2 (\c i -> if c then i - 1 else 0) cs is
-- XXX LH says this is unsafe because i - 1 cannot be shown to be
-- of type Nat. (Removing -1 makes the check pass.)
-- But this is not true: due to the guard on c, we know i is 1.

{-@ rotate :: {xs : [a] | len xs > 0 } -> {ys : [a] | len xs = len ys } @-}
rotate :: [a] -> [a]
rotate xs =
  let n = length xs
  in map (\i -> if i == 0 then xs !! (n - 1) else xs !! (i - 1)) [0 .. n - 1]
-- XXX gets rejected.

{-@ part2Indices :: {cs : [Bool] | True } -> {is : [Nat] | len is = len cs } @-}
part2Indices :: [Bool] -> [Int]
part2Indices cs =
  let n = length cs
      tflgs = map1 (\c -> if c then 1 else 0) cs
      fflgs = map1 (\b -> 1 - b) tflgs
      indsT = scan (+) 0 tflgs
      tmp = scan (+) 0 fflgs
      lst = if n > 0 then indsT !! (n - 1) else 0
      indsF = map1 (\t -> t + lst) tmp
   in map3 (\c indT indF -> if c then indT - 1 else indF - 1) cs indsT indsF
-- XXX gets rejected for same reason as `simple`.

-- TODO Next up: defined permutation proof and use it on post-condition above.
permutation from to xs = undefined
