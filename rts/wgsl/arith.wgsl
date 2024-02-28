// Start of arith.wgsl

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

// End of arith.wgsl
