// Start of scalar32.wgsl

fn read_i32(buffer: ptr<storage, array<i32>, read_write>, i: i32) -> i32 {
  return (*buffer)[i];
}

fn write_i32(buffer: ptr<storage, array<i32>, read_write>, i: i32, val: i32) {
  (*buffer)[i] = val;
}

fn neg_i32(a: i32) -> i32 {
  return -a;
}

fn udiv_i32(a: i32, b: i32) -> i32 {
  return bitcast<i32>(bitcast<u32>(a) / bitcast<u32>(b));
}

fn udiv_up_i32(a_s: i32, b_s: i32) -> i32 {
  let a = bitcast<u32>(a_s);
  let b = bitcast<u32>(b_s);
  return bitcast<i32>((a + b - 1) / b);
}

fn sdiv_i32(a: i32, b: i32) -> i32 {
  let q = a / b;
  let r = a % b;
  if (r != 0 && ((r < 0) != (b < 0))) { return q - 1; }
  return q;
}

fn sdiv_up_i32(a: i32, b: i32) -> i32 {
  return sdiv_i32(a + b - 1, b);
}

fn umod_i32(a: i32, b: i32) -> i32 {
  return bitcast<i32>(bitcast<u32>(a) % bitcast<u32>(b));
}

fn smod_i32(a: i32, b: i32) -> i32 {
  let r = a % b;
  if (r == 0 || (a > 0 && b > 0) || (a < 0 && b < 0)) { return r; }
  return r + b;
}

fn umin_i32(a: i32, b: i32) -> i32 {
  return bitcast<i32>(min(bitcast<u32>(a), bitcast<u32>(b)));
}

fn umax_i32(a: i32, b: i32) -> i32 {
  return bitcast<i32>(max(bitcast<u32>(a), bitcast<u32>(b)));
}

fn shl_i32(a: i32, b: i32) -> i32 {
  return a << bitcast<u32>(b);
}

fn lshr_i32(a: i32, b: i32) -> i32 {
  return bitcast<i32>(bitcast<u32>(a) >> bitcast<u32>(b));
}

fn ashr_i32(a: i32, b: i32) -> i32 {
  return a >> bitcast<u32>(b);
}

fn pow_i32(a_p: i32, b: i32) -> i32 {
  var a = a_p;
  var res: i32 = 1;
  var rem: i32 = b;

  while rem != 0 {
    if (rem & 1) != 0 {
      res = res * a;  
	}
	rem = rem >> 1;
	a = a * a;
  }

  return res;
}

fn ult_i32(a: i32, b: i32) -> bool {
  return bitcast<u32>(a) < bitcast<u32>(b);
}

fn ule_i32(a: i32, b: i32) -> bool {
  return bitcast<u32>(a) <= bitcast<u32>(b);
}

fn usignum_i32(a: i32) -> i32 {
  if a == 0 { return 0; }
  return 1;
}

fn f16_to_u32(a: f16) -> i32 {
  return bitcast<i32>(u32(a));
}

fn f32_to_u32(a: f32) -> i32 {
  return bitcast<i32>(u32(a));
}

fn u32_to_f16(a: i32) -> f16 {
  return f16(bitcast<u32>(a));
}

fn u32_to_f32(a: i32) -> f32 {
  return f32(bitcast<u32>(a));
}

fn f32_inf_helper() -> u32 { return 0x7f800000u; }
fn f32_neg_inf_helper() -> u32 { return 0xff800000u; }
fn f32_nan_helper() -> u32 { return 0xffffffffu; }

fn f32_inf() -> f32 {
  return bitcast<f32>(f32_inf_helper());
}

fn f32_neg_inf() -> f32 {
  return bitcast<f32>(f32_neg_inf_helper());
}

fn f32_nan() -> f32 {
  return bitcast<f32>(f32_nan_helper());
}

// End of scalar32.wgsl
