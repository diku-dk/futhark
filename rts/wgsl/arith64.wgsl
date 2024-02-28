// Start of arith64.wgsl

alias i64 = vec2<i32>; // (low, high)

const zero_i64: i64 = i64(0, 0); 

fn add_i64(a: i64, b: i64) -> i64 {
  // return bitcast<i64>(add_u64(bitcast<u64>(a), bitcast<u64>(b)));
  var r = a + b;
  if (bitcast<u32>(r.x) < bitcast<u32>(a.x)) { r.y += 1; }
  return r;
}

fn neg_i64(a: i64) -> i64 {
  return add_i64(-a, i64(1, 0));
}

fn sub_i64(a: i64, b: i64) -> i64 {
  return add_i64(a, neg_i64(b));
}

fn mul_u32_full(a32: u32, b32: u32) -> i64 {
    let a = vec2(a32 & 0xFFFF, a32 >> 16);
    let b = vec2(b32 & 0xFFFF, b32 >> 16);
	let ll = a.x * b.x; var hh = a.y * b.y;
	let lh = a.x * b.y; var hl = a.y * b.x;
    let mid = hl + (ll >> 16) + (lh & 0xFFFF);
    return bitcast<i64>(vec2(
        (mid << 16) | (ll & 0xFFFF),
        hh + (mid >> 16) + (lh >> 16)
    ));
}

fn mul_i64(a: i64, b: i64) -> i64 {
    return add_i64(
        mul_u32_full(bitcast<u32>(a.x), bitcast<u32>(b.x)),
        i64(0, a.x * b.y + b.x * a.y)
    );
}

fn slt_i64(a: i64, b: i64) -> bool {
  return a.y < b.y || (a.y == b.y && a.x < b.x);
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

// End of arith64.wgsl
