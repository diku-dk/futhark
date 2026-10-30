// Start of scalar64.wgsl

alias i64 = vec2<i32>; // (low, high)

const zero_i64: i64 = i64(0, 0);
const one_i64: i64 = i64(1, 0);

fn read_i64(buffer: ptr<storage, array<i64>, read_write>, i: i32) -> i64 {
  return (*buffer)[i];
}

fn write_i64(buffer: ptr<storage, array<i64>, read_write>, i: i32, val: i64) {
  (*buffer)[i] = val;
}

fn add_i64(a: i64, b: i64) -> i64 {
  // return bitcast<i64>(add_u64(bitcast<u64>(a), bitcast<u64>(b)));
  var r = a + b;
  if (bitcast<u32>(r.x) < bitcast<u32>(a.x)) { r.y += 1; }
  return r;
}

fn neg_i64(a: i64) -> i64 {
  return add_i64(~a, one_i64);
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

// Unsigned division by binary long division, yielding the quotient and the
// remainder.  Division by zero yields an all-ones quotient and the dividend as
// the remainder, but is not otherwise checked.  The common case of 32-bit
// operands, such as most size calculations, uses native division.
struct udivmod_i64_res {
  q: i64,
  r: i64,
}

fn udivmod_i64(a: i64, b: i64) -> udivmod_i64_res {
  if a.y == 0 && b.y == 0 && b.x != 0 {
    return udivmod_i64_res(i64(udiv_i32(a.x, b.x), 0), i64(umod_i32(a.x, b.x), 0));
  }
  var q = zero_i64;
  var r = zero_i64;
  for (var i = 63; i >= 0; i--) {
    // If the top bit of r is set, then the shifted r does not fit in 64 bits,
    // and is certainly at least b.  The wrapping subtraction then gives the
    // right remainder.
    let carry = r.y < 0;
    r = shl_i64(r, one_i64) | (lshr_i64(a, i64(i, 0)) & one_i64);
    if carry || ule_i64(b, r) {
      r = sub_i64(r, b);
      q = q | shl_i64(one_i64, i64(i, 0));
    }
  }
  return udivmod_i64_res(q, r);
}

fn udiv_i64(a: i64, b: i64) -> i64 {
  return udivmod_i64(a, b).q;
}

fn uceil_div_i64(a: i64, b: i64) -> i64 {
  let qr = udivmod_i64(a, b);
  if eq_i64(qr.r, zero_i64) { return qr.q; }
  return add_i64(qr.q, one_i64);
}

fn umod_i64(a: i64, b: i64) -> i64 {
  return udivmod_i64(a, b).r;
}

// The signed operations divide the magnitudes, which are correct when read as
// unsigned even for the most negative value.  Dividing the most negative value
// by -1 then wraps around to itself.

fn squot_i64(a: i64, b: i64) -> i64 {
  let q = udiv_i64(abs_i64(a), abs_i64(b));
  if (a.y < 0) != (b.y < 0) { return neg_i64(q); }
  return q;
}

fn srem_i64(a: i64, b: i64) -> i64 {
  let r = umod_i64(abs_i64(a), abs_i64(b));
  if a.y < 0 { return neg_i64(r); }
  return r;
}

fn sdiv_i64(a: i64, b: i64) -> i64 {
  let q = squot_i64(a, b);
  let r = srem_i64(a, b);
  if !eq_i64(r, zero_i64) && ((r.y < 0) != (b.y < 0)) {
    return sub_i64(q, one_i64);
  }
  return q;
}

fn sceil_div_i64(a: i64, b: i64) -> i64 {
  let q = squot_i64(a, b);
  let r = srem_i64(a, b);
  if !eq_i64(r, zero_i64) && ((r.y < 0) == (b.y < 0)) {
    return add_i64(q, one_i64);
  }
  return q;
}

fn smod_i64(a: i64, b: i64) -> i64 {
  let r = srem_i64(a, b);
  if !eq_i64(r, zero_i64) && ((r.y < 0) != (b.y < 0)) {
    return add_i64(r, b);
  }
  return r;
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

// The shift amount is masked to the bit width.

fn shl_i64(a: i64, b_full: i64) -> i64 {
  let b: u32 = bitcast<u32>(b_full.x & 63);

  if b == 0 { return a; }
  if b >= 32 { return i64(0, a.x << (b - 32)); }

  let shifted_over = bitcast<i32>(bitcast<u32>(a.x) >> (32 - b));
  return i64(a.x << b, (a.y << b) | shifted_over);
}

fn lshr_i64(a: i64, b_full: i64) -> i64 {
  let b: i32 = b_full.x & 63;

  if b == 0 { return a; }
  if b >= 32 { return i64(lshr_i32(a.y, b - 32), 0); }

  let shifted_over = a.y << bitcast<u32>(32 - b);
  return i64(lshr_i32(a.x, b) | shifted_over, lshr_i32(a.y, b));
}

fn ashr_i64(a: i64, b_full: i64) -> i64 {
  let b: u32 = bitcast<u32>(b_full.x & 63);

  if b == 0 { return a; }
  if b >= 32 {
    var high: i32;
    if a.y < 0 { high = -1; } else { high = 0; }
    return i64(a.y >> (b - 32), high);
  }

  let shifted_over = a.y << (32 - b);
  return i64(lshr_i32(a.x, bitcast<i32>(b)) | shifted_over, a.y >> b);
}

fn pow_i64(a_p: i64, b: i64) -> i64 {
  var a = a_p;
  var res: i64 = one_i64;
  var rem: i64 = b;

  while !eq_i64(rem, zero_i64) {
    if !eq_i64(rem & one_i64, zero_i64) {
      res = mul_i64(res, a);
    }
    rem = lshr_i64(rem, one_i64);
    a = mul_i64(a, a);
  }

  return res;
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

// Only the high words are signed.
fn slt_i64(a: i64, b: i64) -> bool {
  return a.y < b.y || (a.y == b.y && bitcast<u32>(a.x) < bitcast<u32>(b.x));
}

fn sle_i64(a: i64, b: i64) -> bool {
  return a.y < b.y || (a.y == b.y && bitcast<u32>(a.x) <= bitcast<u32>(b.x));
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
  return i64(a, 0);
}

fn trunc_i64_i32(a: i64) -> i32 {
  return a.x;
}

fn bool_to_i64(a: bool) -> i64 {
  if a { return one_i64; } else { return zero_i64; }
}

fn i64_to_bool(a: i64) -> bool {
  if eq_i64(a, zero_i64) { return false; } else { return true; }
}

// Conversions between 64-bit integers and floats go through f32, combining
// or splitting the 32-bit words.  This may round twice.

fn u64_to_f32(a: i64) -> f32 {
  return f32(bitcast<u32>(a.y)) * 4294967296.0f + f32(bitcast<u32>(a.x));
}

fn i64_to_f32(a: i64) -> f32 {
  if a.y < 0 { return -u64_to_f32(neg_i64(a)); }
  return u64_to_f32(a);
}

fn u64_to_f16(a: i64) -> f16 {
  return f16(u64_to_f32(a));
}

fn i64_to_f16(a: i64) -> f16 {
  return f16(i64_to_f32(a));
}

fn f32_to_u64(a: f32) -> i64 {
  let hi = floor(a / 4294967296.0f);
  let lo = a - hi * 4294967296.0f;
  return i64(bitcast<i32>(u32(lo)), bitcast<i32>(u32(hi)));
}

fn f32_to_i64(a: f32) -> i64 {
  if a < 0 { return neg_i64(f32_to_u64(-a)); }
  return f32_to_u64(a);
}

fn f16_to_u64(a: f16) -> i64 {
  return f32_to_u64(f32(a));
}

fn f16_to_i64(a: f16) -> i64 {
  return f32_to_i64(f32(a));
}

// End of scalar64.wgsl
