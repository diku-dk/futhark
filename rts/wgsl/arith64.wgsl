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

fn smin_i64(a: i64, b: i64) -> i64 {
  if slt_i64(a, b) { return a; }
  return b;
}

fn umin_i64(a: i64, b: i64) -> i64 {
  if ult_i64(a, b) { return a; }
  return b;
}

fn smax_i64(a: i64, b: i64) -> i64 {
  if slt_i64(a, b) { return b; }
  return a;
}

fn umax_i64(a: i64, b: i64) -> i64 {
  if ult_i64(a, b) { return b; }
  return a;
}

fn shl_i64(a: i64, b_full: i64) -> i64 {
  // Shifting by more than 64 and by negative amounts is undefined, so we can
  // assume b.y is 0 and b.x >= 0.
  let b: u32 = bitcast<u32>(b_full.x);

  if b == 0 { return a; }
  if b >= 32 { return i64(0, a.x << (b - 32)); }

  let shifted_over = bitcast<i32>(bitcast<u32>(a.x) >> (32 - b));
  return i64(a.x << b, (a.y << b) | shifted_over);
}

fn lshr_i64(a: i64, b_full: i64) -> i64 {
  // Shifting by more than 64 and by negative amounts is undefined, so we can
  // assume b.y is 0 and b.x >= 0.
  let b: i32 = b_full.x;

  if b == 0 { return a; }
  if b >= 32 { return i64(lshr_i32(a.y, b - 32), 0); }

  let shifted_over = a.y << bitcast<u32>(32 - b);
  return i64(lshr_i32(a.x, b) | shifted_over, lshr_i32(a.y, b));
}

fn ashr_i64(a: i64, b_full: i64) -> i64 {
  // Shifting by more than 64 and by negative amounts is undefined, so we can
  // assume b.y is 0 and b.x >= 0.
  let b: u32 = bitcast<u32>(b_full.x);

  if b == 0 { return a; }
  if b >= 32 {
	var high: i32;
	if a.y < 0 { high = -1; } else { high = 0; }
    return i64(a.y >> (b - 32), high);
  }

  let shifted_over = a.y << (32 - b);
  return i64(lshr_i32(a.x, bitcast<i32>(b)) | shifted_over, a.y >> b);
}

fn eq_i64(a: i64, b: i64) -> bool {
  return all(a == b);
}

fn ult_i64(a_s: i64, b_s: i64) -> bool {
  let a = bitcast<vec2<u32>>(a_s);
  let b = bitcast<vec2<u32>>(b_s);
  return a.y < b.y || (a.y == b.y && a.x < b.x);
}

fn ule_i64(a_s: i64, b_s: i64) -> bool {
  let a = bitcast<vec2<u32>>(a_s);
  let b = bitcast<vec2<u32>>(b_s);
  return a.y < b.y || (a.y == b.y && a.x <= b.x);
}

fn slt_i64(a: i64, b: i64) -> bool {
  return a.y < b.y || (a.y == b.y && a.x < b.x);
}

fn sle_i64(a: i64, b: i64) -> bool {
  return a.y < b.y || (a.y == b.y && a.x <= b.x);
}

fn abs_i64(a: i64) -> i64 {
  if slt_i64(a, zero_i64) { return neg_i64(a); }
  return a;
}

fn ssignum_i64(a: i64) -> i64 {
  if slt_i64(a, zero_i64) { return i64(-1, -1); }
  if all(a == zero_i64) { return i64(0, 0); }
  return i64(1, 0);
}

fn usignum_i64(a: i64) -> i64 {
  if all(a == zero_i64) { return i64(0, 0); }
  return i64(1, 0);
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
