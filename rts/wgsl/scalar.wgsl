// Start of scalar.wgsl

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

fn log_and(a: bool, b: bool) -> bool {
  return a && b;
}

fn log_or(a: bool, b: bool) -> bool {
  return a || b;
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

fn futrts_log32(a: f32) -> f32 {
  return log(a);
}

fn futrts_log2_32(a: f32) -> f32 {
  return log2(a);
}

fn futrts_log10_32(a: f32) -> f32 {
  return log(a) / log(10);
}

fn futrts_log1p_32(a: f32) -> f32 {
  return log(1.0 + a);
}

fn futrts_sqrt32(a: f32) -> f32 {
  return sqrt(a);
}

//fn futrts_cbrt32(a: f32) -> f32 {
//  return ???;
//}

fn futrts_exp32(a: f32) -> f32 {
  return exp(a);
}

fn futrts_cos32(a: f32) -> f32 {
  return cos(a);
}

fn futrts_sin32(a: f32) -> f32 {
  return sin(a);
}

fn futrts_tan32(a: f32) -> f32 {
  return tan(a);
}

fn futrts_acos32(a: f32) -> f32 {
  return acos(a);
}

fn futrts_asin32(a: f32) -> f32 {
  return asin(a);
}

fn futrts_atan32(a: f32) -> f32 {
  return atan(a);
}

fn futrts_cosh32(a: f32) -> f32 {
  return cosh(a);
}

fn futrts_sinh32(a: f32) -> f32 {
  return sinh(a);
}

fn futrts_tanh32(a: f32) -> f32 {
  return tanh(a);
}

fn futrts_acosh32(a: f32) -> f32 {
  return acosh(a);
}

fn futrts_asinh32(a: f32) -> f32 {
  return asinh(a);
}

fn futrts_atanh32(a: f32) -> f32 {
  return atanh(a);
}

fn futrts_atan2_32(a: f32, b: f32) -> f32 {
  return atan2(a, b);
}

// End of scalar.wgsl
