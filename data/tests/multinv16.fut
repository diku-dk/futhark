-- Multiplicative inverse on 16-bit numbers.  Returned as a 32-bit
-- number to print better (because we do not have unsigned types).  At
-- one point the compiler missimplified the convergence loop.
--
-- ==
-- input { 2i16 } output { 32769i32 }

fun i32 main(i16 a) =
  let b = 0x10001 in
  let u = 0 in
  let v = 1 in
  loop ({a,b,u,v}) = while a > 0i16 do
    let q = i32((i64(b)&0xFFFFFFFFi64) // (i64(a)&0xFFFFi64)) in
    let r = i32((i64(b)&0xFFFFFFFFi64) %% (i64(a)&0xFFFFi64)) in

    let b = i32(a)&0xFFFFFFFF in
    let a = i16(r) in

    let t = v in
    let v = u - q * v in
    let u = t in
    {a,b,u,v} in

  (if u < 0 then u + 0x10001 else u) & 0xFFFF
