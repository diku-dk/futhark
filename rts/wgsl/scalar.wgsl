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

fn futrts_atan2_32(a: f32, b: f32) -> f32 { if (a == 0 && b == 0) { return 0; } return atan2(a, b); }
fn futrts_atan2_16(a: f16, b: f16) -> f16 { if (a == 0 && b == 0) { return 0; } return atan2(a, b); }

fn futrts_atan2pi_32(a: f32, b: f32) -> f32 { return futrts_atan2_32(a, b) / 3.14159265358979323846; }
fn futrts_atan2pi_16(a: f16, b: f16) -> f16 { return futrts_atan2_16(a, b) / 3.14159265358979323846; }

fn futrts_to_bits16(a: f16) -> i16 { return bitcast<i32>(vec2<f16>(a, 0.0)); }
fn futrts_from_bits16(a: i16) -> f16 { return bitcast<vec2<f16>>(a)[0]; }

fn futrts_to_bits32(a: f32) -> i32 { return bitcast<i32>(a); }
fn futrts_from_bits32(a: i32) -> f32 { return bitcast<f32>(a); }

fn futrts_round32(x: f32) -> f32 { return round(x); }
fn futrts_round16(x: f16) -> f16 { return round(x); }

fn futrts_lerp32(a: f32, b: f32, t: f32) -> f32 { return mix(a, b, t); }
fn futrts_lerp16(a: f16, b: f16, t: f16) -> f16 { return mix(a, b, t); }

fn futrts_mad32(a: f32, b: f32, c: f32) -> f32 { return a * b + c; }
fn futrts_mad16(a: f16, b: f16, c: f16) -> f16 { return a * b + c; }

fn futrts_fma32(a: f32, b: f32, c: f32) -> f32 { return fma(a, b, c); }
fn futrts_fma16(a: f16, b: f16, c: f16) -> f16 { return fma(a, b, c); }

fn futrts_popc64(a: i64) -> i32 { return countOneBits(a.x) + countOneBits(a.y); }
fn futrts_popc32(a: i32) -> i32 { return countOneBits(a); }
fn futrts_popc16(a: i16) -> i32 { return countOneBits(a & 0xffff); }
fn futrts_popc8(a: i8)  -> i32 { return countOneBits(a & 0xff); }

// TODO: mul_hi32 and 64 cannot currently be implemented properly.
fn futrts_umul_hi8(a: i8, b: i8) -> i8 { return norm_u8((norm_u8(a) * norm_u8(b)) >> 8); }
fn futrts_umul_hi16(a: i16, b: i16) -> i16 { return norm_u16((norm_u16(a) * norm_u16(b)) >> 16); }
fn futrts_umul_hi32(a: i32, b: i32) -> i32 { return bitcast<i32>(bitcast<u32>(a) * bitcast<u32>(b)); }
fn futrts_umul_hi64(a: i64, b: i64) -> i64 { return i64(mul_i64(a, b)[1]); }
fn futrts_smul_hi8(a: i8, b: i8) -> i8 { return norm_i8((a * b) >> 8); }
fn futrts_smul_hi16(a: i16, b: i16) -> i16 { return norm_i16((a * b) >> 16); }
fn futrts_smul_hi32(a: i32, b: i32) -> i32 { return a * b; }
fn futrts_smul_hi64(a: i64, b: i64) -> i64 { return i64(mul_i64(a, b)[1]); }

fn futrts_umad_hi8(a: i8, b: i8, c: i8) -> i8 { return norm_u8(futrts_umul_hi8(a, b) + norm_u8(c)); }
fn futrts_umad_hi16(a: i16, b: i16, c: i16) -> i16 { return norm_u16(futrts_umul_hi16(a, b) + norm_u16(c)); }
fn futrts_umad_hi32(a: i32, b: i32, c: i32) -> i32 { return bitcast<i32>(bitcast<u32>(futrts_umul_hi32(a, b)) + bitcast<u32>(c)); }
fn futrts_umad_hi64(a: i64, b: i64, c: i64) -> i64 { return add_i64(futrts_umul_hi64(a, b), c); }
fn futrts_smad_hi8(a: i8, b: i8, c: i8) -> i8 { return norm_i8(futrts_smul_hi8(a, b) + c); }
fn futrts_smad_hi16(a: i16, b: i16, c: i16) -> i16 { return norm_i16(futrts_smul_hi16(a, b) + c); }
fn futrts_smad_hi32(a: i32, b: i32, c: i32) -> i32 { return futrts_smul_hi32(a, b) + c; }
fn futrts_smad_hi64(a: i64, b: i64, c: i64) -> i64 { return add_i64(futrts_smul_hi64(a, b), c); }

fn futrts_clzz8(x: i8) -> i32 { return countLeadingZeros(x & 0xff) - 24; }
fn futrts_clzz16(x: i16) -> i32 { return countLeadingZeros(x & 0xffff) - 16; }
fn futrts_clzz32(x: i32) -> i32 { return countLeadingZeros(x); }
fn futrts_clzz64(x: i64) -> i32 { 
    if (x[1] == 0) {
        return countLeadingZeros(x[0]) + 32;
    }
    else {
        return countLeadingZeros(x[1]);
    }
}

fn futrts_ctzz8(x: i8) -> i32 { return min(8, countTrailingZeros(x & 0xff)); }
fn futrts_ctzz16(x: i16) -> i32 { return min(16, countTrailingZeros(x & 0xffff)); }
fn futrts_ctzz32(x: i32) -> i32 { return countTrailingZeros(x); }
fn futrts_ctzz64(x: i64) -> i32 { 
    if (x[0] == 0) {
        return countTrailingZeros(x[1]) + 32;
    }
    else {
        return countTrailingZeros(x[0]);
    }
}