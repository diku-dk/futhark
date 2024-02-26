// Start of arith64.wgsl

alias u64 = vec2<u32>; // (low, high)
alias i64 = vec2<i32>; // (low, high)

fn add_u64(a: u64, b: u64) -> u64 {
  var r: u64;
  r.x = a.x + b.x;
  r.y = a.y + b.y;
  if (r.x < a.x) { r.y += 1; }
  return r;
}

fn add_i64(a: i64, b: i64) -> i64 {
  return bitcast<i64>(add_u64(bitcast<u64>(a), bitcast<u64>(b)));
}

fn neg_i64(a: i64) -> i64 {
  return add_i64(-a, i64(1, 0));
}

fn sub_i64(a: i64, b: i64) -> i64 {
  return add_i64(a, neg_i64(b));
}

fn sub_u64(a: u64, b: u64) -> u64 {
  return bitcast<u64>(sub_i64(bitcast<i64>(a), bitcast<i64>(b)));
}

fn zext_i32_i64(a: i32) -> i64 {
  return i64(a, 0);
}

fn sext_i32_i64(a: i32) -> i64 {
  if (a < 0) { return i64(a, -1); }
  else { return i64(a, 0); }
}

fn trunc_i64_i32(a: i64) -> i32 {
	return a.x;
}

fn slt_i64(a: i64, b: i64) -> bool {
    return a.y < b.y || (a.y == b.y && a.x < b.x);
}

// End of arith64.wgsl
