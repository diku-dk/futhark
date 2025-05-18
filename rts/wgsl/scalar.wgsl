fn log_and(a: bool, b: bool) -> bool { return a && b; }
fn log_or(a: bool, b: bool) -> bool { return a || b; }
fn llt(a: bool, b: bool) -> bool { return a == false && b == true; }
fn lle(a: bool, b: bool) -> bool { return a == b || llt(a, b); }


fn futrts_sqrt32(a: f32) -> f32 { return sqrt(a); }
fn futrts_sqrt16(a: f16) -> f16 { return sqrt(a); }

fn futrts_rsqrt32(a: f32) -> f32 { return inverseSqrt(a); }
fn futrts_rsqrt16(a: f16) -> f16 { return inverseSqrt(a); }

//fn futrts_cbrt32(a: f32) -> f32 { return ???; }
//fn futrts_cbrt16(a: f16) -> f16 { return ???; }

fn futrts_log32(a: f32) -> f32 { return log(a); }
fn futrts_log16(a: f16) -> f16 { return log(a); }

fn futrts_log10_32(a: f32) -> f32 { return log(a) / log(10); }
fn futrts_log10_16(a: f16) -> f16 { return log(a) / log(10); }

fn futrts_log1p_32(a: f32) -> f32 { return log(1.0 + a); }
fn futrts_log1p_16(a: f16) -> f16 { return log(1.0 + a); }

fn futrts_log2_32(a: f32) -> f32 { return log2(a); }
fn futrts_log2_16(a: f16) -> f16 { return log2(a); }

fn futrts_exp32(a: f32) -> f32 { return exp(a); }
fn futrts_exp16(a: f16) -> f16 { return exp(a); }

fn futrts_sin32(a: f32) -> f32 { return sin(a); }
fn futrts_sin16(a: f16) -> f16 { return sin(a); }

fn futrts_sinpi32(a: f32) -> f32 { return sin(a * 3.14159265358979323846); }
fn futrts_sinpi16(a: f16) -> f16 { return sin(a * 3.14159265358979323846); }

fn futrts_sinh32(a: f32) -> f32 { return sinh(a); }
fn futrts_sinh16(a: f16) -> f16 { return sinh(a); }

fn futrts_cos32(a: f32) -> f32 { return cos(a); }
fn futrts_cos16(a: f16) -> f16 { return cos(a); }

fn futrts_cospi32(a: f32) -> f32 { return cos(a * 3.14159265358979323846); }
fn futrts_cospi16(a: f16) -> f16 { return cos(a * 3.14159265358979323846); }

fn futrts_cosh32(a: f32) -> f32 { return cosh(a); }
fn futrts_cosh16(a: f16) -> f16 { return cosh(a); }

fn futrts_tan32(a: f32) -> f32 { return tan(a); }
fn futrts_tan16(a: f16) -> f16 { return tan(a); }

fn futrts_tanpi32(a: f32) -> f32 { return tan(a * 3.14159265358979323846); }
fn futrts_tanpi16(a: f16) -> f16 { return tan(a * 3.14159265358979323846); }

fn futrts_tanh32(a: f32) -> f32 { return tanh(a); }
fn futrts_tanh16(a: f16) -> f16 { return tanh(a); }

fn futrts_asin32(a: f32) -> f32 { return asin(a); }
fn futrts_asin16(a: f16) -> f16 { return asin(a); }

fn futrts_asinpi32(a: f32) -> f32 { return asin(a) / 3.14159265358979323846; }
fn futrts_asinpi16(a: f16) -> f16 { return asin(a) / 3.14159265358979323846; }

fn futrts_asinh32(a: f32) -> f32 { return asinh(a); }
fn futrts_asinh16(a: f16) -> f16 { return asinh(a); }

fn futrts_acos32(a: f32) -> f32 { return acos(a); }
fn futrts_acos16(a: f16) -> f16 { return acos(a); }

fn futrts_acospi32(a: f32) -> f32 { return acos(a) / 3.14159265358979323846; }
fn futrts_acospi16(a: f16) -> f16 { return acos(a) / 3.14159265358979323846; }

fn futrts_acosh32(a: f32) -> f32 { return acosh(a); }
fn futrts_acosh16(a: f16) -> f16 { return acosh(a); }

fn futrts_atan32(a: f32) -> f32 { return atan(a); }
fn futrts_atan16(a: f16) -> f16 { return atan(a); }

fn futrts_atanpi32(a: f32) -> f32 { return atan(a) / 3.14159265358979323846; }
fn futrts_atanpi16(a: f16) -> f16 { return atan(a) / 3.14159265358979323846; }

fn futrts_atanh32(a: f32) -> f32 { return atanh(a); }
fn futrts_atanh16(a: f16) -> f16 { return atanh(a); }

fn futrts_round_32(a: f32) -> f32 { return round(a); }
fn futrts_round_16(a: f16) -> f16 { return round(a); }

fn futrts_ceil32(a: f32) -> f32 { return ceil(a); }
fn futrts_ceil16(a: f16) -> f16 { return ceil(a); }

fn futrts_floor32(a: f32) -> f32 { return floor(a); }
fn futrts_floor16(a: f16) -> f16 { return floor(a); }

fn futrts_ldexp32(a: f32, b: i32) -> f32 { return ldexp(a, b); }
fn futrts_ldexp16(a: f16, b: i32) -> f16 { return ldexp(a, b); }

fn futrts_clz32(a: i32) -> i32 { return countLeadingZeros(a); }
fn futrts_ctz32(a: i32) -> i32 { return countTrailingZeros(a); }
fn futrts_popc32(a: i32) -> i32 { return countOneBits(a); }

// TODO: umad, umul, smad, smul

fn futrts_atan2_32(a: f32, b: f32) -> f32 { if (a == 0 && b == 0) { return 0; } return atan2(a, b); }
fn futrts_atan2_16(a: f16, b: f16) -> f16 { if (a == 0 && b == 0) { return 0; } return atan2(a, b); }

fn futrts_atan2pi_32(a: f32, b: f32) -> f32 { return futrts_atan2_32(a, b) / 3.14159265358979323846; }
fn futrts_atan2pi_16(a: f16, b: f16) -> f16 { return futrts_atan2_16(a, b) / 3.14159265358979323846; }

// TODO: f16 -> i16, i16 -> f16
fn futrts_to_bits32(a: f32) -> i32 { return bitcast<i32>(a); }
fn futrts_from_bits32(a: i32) -> f32 { return bitcast<f32>(a); }

// TODO: are these equivalent to the Futhark definitions?
fn futrts_lerp32(a: f32, b: f32, t: f32) -> f32 { return mix(a, b, t); }
fn futrts_lerp16(a: f16, b: f16, t: f16) -> f16 { return mix(a, b, t); }

fn futrts_mad32(a: f32, b: f32, c: f32) -> f32 { return a * b + c; }
fn futrts_mad16(a: f16, b: f16, c: f16) -> f16 { return a * b + c; }

fn futrts_fma32(a: f32, b: f32, c: f32) -> f32 { return fma(a, b, c); }
fn futrts_fma16(a: f16, b: f16, c: f16) -> f16 { return fma(a, b, c); }
