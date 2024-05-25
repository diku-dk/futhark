// Start of scalar16.wgsl

alias i16 = i32;

fn norm_i16(a: i16) -> i32 {
  if (a & 0x8000) != 0 { return a | bitcast<i32>(0xffff0000u); }
  return a & 0x0000ffff;
}

fn norm_u16(a: i16) -> i32 {
  return a & 0x0000ffff;
}

fn read_i16(buffer: ptr<storage, array<atomic<i16>>, read_write>, i: i32) -> i16 {
  let elem_idx = i / 2;
  let idx_in_elem = i % 2;

  let v = atomicLoad(&((*buffer)[elem_idx]));
  return norm_i16(v >> bitcast<u32>(idx_in_elem * 16));
}

fn write_i16(buffer: ptr<storage, array<atomic<i16>>, read_write>,
             i: i32,
             val: i16
) {
  let elem_idx = i / 2;
  let idx_in_elem = i % 2;

  let shift_amt = bitcast<u32>(idx_in_elem * 16);

  let mask = 0xffff << shift_amt;
  let shifted_val = (val << shift_amt) & mask;

  // First zero out the previous value using the inverted mask.
  atomicAnd(&((*buffer)[elem_idx]), ~mask);
  // And then write the new value.
  atomicOr(&((*buffer)[elem_idx]), shifted_val);
}

fn add_i16(a: i16, b: i16) -> i16 {
  return norm_i16(a + b);
}

fn neg_i16(a: i16) -> i16 {
  return add_i16(~a, 1);
}

fn sub_i16(a: i16, b: i16) -> i16 {
  return add_i16(a, neg_i16(b));
}

fn mul_i16(a: i16, b: i16) -> i16 {
  return norm_i16(a * b);
}

fn udiv_i16(a: i16, b: i16) -> i16 {
  return norm_i16(udiv_i32(norm_u16(a), norm_u16(b)));
}

fn udiv_up_i16(a: i16, b: i16) -> i16 {
  return norm_i16(udiv_up_i32(norm_u16(a), norm_u16(b)));
}

fn sdiv_i16(a: i16, b: i16) -> i16 {
  return sdiv_i32(a, b);
}

fn sdiv_up_i16(a: i16, b: i16) -> i16 {
  return sdiv_up_i32(a, b);
}

fn umod_i16(a: i16, b: i16) -> i16 {
  return norm_i16(umod_i32(norm_u16(a), norm_u16(b)));
}

fn smod_i16(a: i16, b: i16) -> i16 {
  return smod_i32(a, b);
}

fn umin_i16(a: i16, b: i16) -> i16 {
  return umin_i32(a, b);
}

fn umax_i16(a: i16, b: i16) -> i16 {
  return umax_i32(a, b);
}

fn shl_i16(a: i16, b: i16) -> i16 {
  return a << bitcast<u32>(b);
}

fn lshr_i16(a: i16, b: i16) -> i16 {
  return bitcast<i16>(bitcast<u32>(a) >> bitcast<u32>(b));
}

fn ashr_i16(a: i16, b: i16) -> i16 {
  return a >> bitcast<u32>(b);
}

fn pow_i16(a_p: i16, b: i16) -> i16 {
  var a = a_p;
  var res: i16 = 1;
  var rem: i16 = b;

  while rem != 0 {
    if (rem & 1) != 0 {
	  res = mul_i16(res, a);
	}
	rem = ashr_i16(rem, 1);
	a = mul_i16(a, a);
  }

  return res;
}

fn zext_i16_i32(a: i16) -> i32 {
  return a & 0xffff;
}

// Our representation is already a sign-extended i32.
fn sext_i16_i32(a: i16) -> i32 {
  return a;
}

fn zext_i16_i64(a: i16) -> i64 {
  return i64(zext_i16_i32(a), 0);
}

fn sext_i16_i64(a: i16) -> i64 {
  return sext_i32_i64(a);
}

fn trunc_i32_i16(a: i32) -> i16 {
  return a & 0xffff;
}

fn trunc_i64_i16(a: i64) -> i16 {
  return trunc_i32_i16(a.x);
}

fn u16_to_f16(a: i16) -> f16 {
  return f16(bitcast<u32>(a));
}

fn u16_to_f32(a: i16) -> f32 {
  return f32(bitcast<u32>(a));
}

fn bool_to_i16(a: bool) -> i16 {
  if a { return 1; } else { return 0; }
}

// End of scalar16.wgsl
