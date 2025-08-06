-- In-place lowering should be careful about updates where the new
-- value potentially aliases the old one.

type t = [8]u32

def pack [n] (zero: t) (xs: [n]bool) : t =
  loop ret = zero
  for i in 0..<n do
    if xs[i]
    then map1 (+ 1) ret
    else ret

def main =
  let zero: t = replicate 8 0
  in map (\x -> replicate 2 (x >= 4) |> pack zero) (iota 10)
