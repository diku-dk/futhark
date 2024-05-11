// Start of scalar.wgsl

fn log_and(a: bool, b: bool) -> bool {
  return a && b;
}

fn log_or(a: bool, b: bool) -> bool {
  return a || b;
}

fn llt(a: bool, b: bool) -> bool {
  return a == false && b == true;
}

fn lle(a: bool, b: bool) -> bool {
  return a == b || llt(a, b);
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
