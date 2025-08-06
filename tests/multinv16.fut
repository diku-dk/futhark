-- Multiplicative inverse on 16-bit numbers.  Returned as a 32-bit
-- number to print better (because we do not print unsigned types).
-- At one point the compiler missimplified the convergence loop.
--
-- ==
-- input { 2u16 } output { 32769u32 }
-- input { 33799u16 } output { 28110u32 }

def main (a: u16) : u32 =
  let b = 0x10001u32
  let u = 0i32
  let v = 1i32
  let (_, _, u, _) =
    loop ((a, b, u, v)) while a > 0u16 do
      let q = b / u32.u16 (a)
      let r = b % u32.u16 (a)
      let b = u32.u16 (a)
      let a = u16.u32 (r)
      let t = v
      let v = u - i32.u32 (q) * v
      let u = t
      in (a, b, u, v)
  in u32.i32 (if u < 0 then u + 0x10001 else u)
