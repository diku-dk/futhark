// Start of arith.wgsl

fn umin_i32(a: i32, b: i32) -> i32 {
  return bitcast<i32>(min(bitcast<u32>(a), bitcast<u32>(b)));
}

fn umax_i32(a: i32, b: i32) -> i32 {
  return bitcast<i32>(max(bitcast<u32>(a), bitcast<u32>(b)));
}

fn lshr_i32(a: i32, b: i32) -> i32 {
  return bitcast<i32>(bitcast<u32>(a) << bitcast<u32>(b));
}

fn ult_i32(a: i32, b: i32) -> bool {
  return bitcast<u32>(a) < bitcast<u32>(b);
}

fn ule_i32(a: i32, b: i32) -> bool {
  return bitcast<u32>(a) <= bitcast<u32>(b);
}

fn llt(a: bool, b: bool) -> bool {
  return a == false && b == true;
}

fn lle(a: bool, b: bool) -> bool {
  return a == b || llt(a, b);
}

fn usignum_i32(a: i32) -> i32 {
  if a == 0 { return 0; }
  return 1;
}

// End of arith.wgsl
