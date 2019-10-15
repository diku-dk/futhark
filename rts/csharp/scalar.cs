// Scalar functions.
private static sbyte signed(byte x){ return (sbyte) x;}
private static short signed(ushort x){ return (short) x;}
private static int signed(uint x){ return (int) x;}
private static long signed(ulong x){ return (long) x;}

private static byte unsigned(sbyte x){ return (byte) x;}
private static ushort unsigned(short x){ return (ushort) x;}
private static uint unsigned(int x){ return (uint) x;}
private static ulong unsigned(long x){ return (ulong) x;}

private static sbyte add8(sbyte x, sbyte y){ return (sbyte) ((byte) x + (byte) y);}
private static short add16(short x, short y){ return (short) ((ushort) x + (ushort) y);}
private static int add32(int x, int y){ return (int) ((uint) x + (uint) y);}
private static long add64(long x, long y){ return (long) ((ulong) x + (ulong) y);}

private static sbyte sub8(sbyte x, sbyte y){ return (sbyte) ((byte) x - (byte) y);}
private static short sub16(short x, short y){ return (short) ((ushort) x - (ushort) y);}
private static int sub32(int x, int y){ return (int) ((uint) x - (uint) y);}
private static long sub64(long x, long y){ return (long) ((ulong) x - (ulong) y);}

private static sbyte mul8(sbyte x, sbyte y){ return (sbyte) ((byte) x * (byte) y);}
private static short mul16(short x, short y){ return (short) ((ushort) x * (ushort) y);}
private static int mul32(int x, int y){ return (int) ((uint) x * (uint) y);}
private static long mul64(long x, long y){ return (long) ((ulong) x * (ulong) y);}

private static sbyte or8(sbyte x, sbyte y){ return (sbyte) (x | y); }
private static short or16(short x, short y){ return (short) (x | y); }
private static int or32(int x, int y){ return x | y; }
private static long or64(long x, long y){ return x | y;}

private static sbyte xor8(sbyte x, sbyte y){ return (sbyte) (x ^ y); }
private static short xor16(short x, short y){ return (short) (x ^ y); }
private static int xor32(int x, int y){ return x ^ y; }
private static long xor64(long x, long y){ return x ^ y;}

private static sbyte and8(sbyte x, sbyte y){ return (sbyte) (x & y); }
private static short and16(short x, short y){ return (short) (x & y); }
private static int and32(int x, int y){ return x & y; }
private static long and64(long x, long y){ return x & y;}

private static sbyte shl8(sbyte x, sbyte y){ return (sbyte) (x << y); }
private static short shl16(short x, short y){ return (short) (x << y); }
private static int shl32(int x, int y){ return x << y; }
private static long shl64(long x, long y){ return x << Convert.ToInt32(y); }

private static sbyte ashr8(sbyte x, sbyte y){ return (sbyte) (x >> y); }
private static short ashr16(short x, short y){ return (short) (x >> y); }
private static int ashr32(int x, int y){ return x >> y; }
private static long ashr64(long x, long y){ return x >> Convert.ToInt32(y); }

private static sbyte sdiv8(sbyte x, sbyte y){
    var q = squot8(x,y);
    var r = srem8(x,y);
    return (sbyte) (q - (((r != (sbyte) 0) && ((r < (sbyte) 0) != (y < (sbyte) 0))) ? (sbyte) 1 : (sbyte) 0));
}
private static short sdiv16(short x, short y){
    var q = squot16(x,y);
    var r = srem16(x,y);
    return (short) (q - (((r != (short) 0) && ((r < (short) 0) != (y < (short) 0))) ? (short) 1 : (short) 0));
}
private static int sdiv32(int x, int y){
    var q = squot32(x,y);
    var r = srem32(x,y);
    return q - (((r != (int) 0) && ((r < (int) 0) != (y < (int) 0))) ? (int) 1 : (int) 0);
}
private static long sdiv64(long x, long y){
    var q = squot64(x,y);
    var r = srem64(x,y);
    return q - (((r != (long) 0) && ((r < (long) 0) != (y < (long) 0))) ? (long) 1 : (long) 0);
}

private static sbyte smod8(sbyte x, sbyte y){
    var r = srem8(x,y);
    return (sbyte) (r + ((r == (sbyte) 0 || (x > (sbyte) 0 && y > (sbyte) 0) || (x < (sbyte) 0 && y < (sbyte) 0)) ? (sbyte) 0 : y));
}
private static short smod16(short x, short y){
    var r = srem16(x,y);
    return (short) (r + ((r == (short) 0 || (x > (short) 0 && y > (short) 0) || (x < (short) 0 && y < (short) 0)) ? (short) 0 : y));
}
private static int smod32(int x, int y){
    var r = srem32(x,y);
    return (int) r + ((r == (int) 0 || (x > (int) 0 && y > (int) 0) || (x < (int) 0 && y < (int) 0)) ? (int) 0 : y);
}
private static long smod64(long x, long y){
    var r = srem64(x,y);
    return (long) r + ((r == (long) 0 || (x > (long) 0 && y > (long) 0) || (x < (long) 0 && y < (long) 0)) ? (long) 0 : y);
}

private static sbyte udiv8(sbyte x, sbyte y){ return signed((byte) (unsigned(x) / unsigned(y))); }
private static short udiv16(short x, short y){ return signed((ushort) (unsigned(x) / unsigned(y))); }
private static int udiv32(int x, int y){ return signed(unsigned(x) / unsigned(y)); }
private static long udiv64(long x, long y){ return signed(unsigned(x) / unsigned(y)); }

private static sbyte umod8(sbyte x, sbyte y){ return signed((byte) (unsigned(x) % unsigned(y))); }
private static short umod16(short x, short y){ return signed((ushort) (unsigned(x) % unsigned(y))); }
private static int umod32(int x, int y){ return signed(unsigned(x) % unsigned(y)); }
private static long umod64(long x, long y){ return signed(unsigned(x) % unsigned(y)); }

private static sbyte squot8(sbyte x, sbyte y){ return (sbyte) Math.Truncate(ToSingle(x) / ToSingle(y)); }
private static short squot16(short x, short y){ return (short) Math.Truncate(ToSingle(x) / ToSingle(y)); }
private static int squot32(int x, int y){ return (int) Math.Truncate(ToSingle(x) / ToSingle(y)); }
private static long squot64(long x, long y){ return (long) Math.Truncate(ToSingle(x) / ToSingle(y)); }

// private static Maybe change srem, it calls np.fmod originally so i dont know
private static sbyte srem8(sbyte x, sbyte y){ return (sbyte) ((sbyte) x % (sbyte) y);}
private static short srem16(short x, short y){ return (short) ((short) x % (short) y);}
private static int srem32(int x, int y){ return (int) ((int) x % (int) y);}
private static long srem64(long x, long y){ return (long) ((long) x % (long) y);}

private static sbyte smin8(sbyte x, sbyte y){ return Math.Min(x,y);}
private static short smin16(short x, short y){ return Math.Min(x,y);}
private static int smin32(int x, int y){ return Math.Min(x,y);}
private static long smin64(long x, long y){ return Math.Min(x,y);}

private static sbyte smax8(sbyte x, sbyte y){ return Math.Max(x,y);}
private static short smax16(short x, short y){ return Math.Max(x,y);}
private static int smax32(int x, int y){ return Math.Max(x,y);}
private static long smax64(long x, long y){ return Math.Max(x,y);}

private static sbyte umin8(sbyte x, sbyte y){ return signed(Math.Min(unsigned(x),unsigned(y)));}
private static short umin16(short x, short y){ return signed(Math.Min(unsigned(x),unsigned(y)));}
private static int umin32(int x, int y){ return signed(Math.Min(unsigned(x),unsigned(y)));}
private static long umin64(long x, long y){ return signed(Math.Min(unsigned(x),unsigned(y)));}

private static sbyte umax8(sbyte x, sbyte y){ return signed(Math.Max(unsigned(x),unsigned(y)));}
private static short umax16(short x, short y){ return signed(Math.Max(unsigned(x),unsigned(y)));}
private static int umax32(int x, int y){ return signed(Math.Max(unsigned(x),unsigned(y)));}
private static long umax64(long x, long y){ return signed(Math.Max(unsigned(x),unsigned(y)));}

private static float fmin32(float x, float y){ return Math.Min(x,y);}
private static double fmin64(double x, double y){ return Math.Min(x,y);}
private static float fmax32(float x, float y){ return Math.Max(x,y);}
private static double fmax64(double x, double y){ return Math.Max(x,y);}

private static sbyte pow8(sbyte x, sbyte y){sbyte res = 1;for (var i = 0; i < y; i++){res *= x;}return res;}
private static short pow16(short x, short y){short res = 1;for (var i = 0; i < y; i++){res *= x;}return res;}
private static int pow32(int x, int y){int res = 1;for (var i = 0; i < y; i++){res *= x;}return res;}
private static long pow64(long x, long y){long res = 1;for (var i = 0; i < y; i++){res *= x;}return res;}

private static float fpow32(float x, float y){ return Convert.ToSingle(Math.Pow(x,y));}
private static double fpow64(double x, double y){ return Convert.ToDouble(Math.Pow(x,y));}

private static bool sle8(sbyte x, sbyte y){ return x <= y ;}
private static bool sle16(short x, short y){ return x <= y ;}
private static bool sle32(int x, int y){ return x <= y ;}
private static bool sle64(long x, long y){ return x <= y ;}

private static bool slt8(sbyte x, sbyte y){ return x < y ;}
private static bool slt16(short x, short y){ return x < y ;}
private static bool slt32(int x, int y){ return x < y ;}
private static bool slt64(long x, long y){ return x < y ;}

private static bool ule8(sbyte x, sbyte y){ return unsigned(x) <= unsigned(y) ;}
private static bool ule16(short x, short y){ return unsigned(x) <= unsigned(y) ;}
private static bool ule32(int x, int y){ return unsigned(x) <= unsigned(y) ;}
private static bool ule64(long x, long y){ return unsigned(x) <= unsigned(y) ;}

private static bool ult8(sbyte x, sbyte y){ return unsigned(x) < unsigned(y) ;}
private static bool ult16(short x, short y){ return unsigned(x) < unsigned(y) ;}
private static bool ult32(int x, int y){ return unsigned(x) < unsigned(y) ;}
private static bool ult64(long x, long y){ return unsigned(x) < unsigned(y) ;}

private static sbyte lshr8(sbyte x, sbyte y){ return (sbyte) ((uint) x >> (int) y);}
private static short lshr16(short x, short y){ return (short) ((ushort) x >> (int) y);}
private static int lshr32(int x, int y){ return (int) ((uint) (x) >> (int) y);}
private static long lshr64(long x, long y){ return (long) ((ulong) x >> (int) y);}

private static sbyte sext_i8_i8(sbyte x){return (sbyte) (x);}
private static short sext_i8_i16(sbyte x){return (short) (x);}
private static int sext_i8_i32(sbyte x){return (int) (x);}
private static long sext_i8_i64(sbyte x){return (long) (x);}

private static sbyte sext_i16_i8(short x){return (sbyte) (x);}
private static short sext_i16_i16(short x){return (short) (x);}
private static int sext_i16_i32(short x){return (int) (x);}
private static long sext_i16_i64(short x){return (long) (x);}

private static sbyte sext_i32_i8(int x){return (sbyte) (x);}
private static short sext_i32_i16(int x){return (short) (x);}
private static int sext_i32_i32(int x){return (int) (x);}
private static long sext_i32_i64(int x){return (long) (x);}

private static sbyte sext_i64_i8(long x){return (sbyte) (x);}
private static short sext_i64_i16(long x){return (short) (x);}
private static int sext_i64_i32(long x){return (int) (x);}
private static long sext_i64_i64(long x){return (long) (x);}

private static sbyte btoi_bool_i8 (bool x){return (sbyte) (Convert.ToInt32(x));}
private static short btoi_bool_i16(bool x){return (short) (Convert.ToInt32(x));}
private static int   btoi_bool_i32(bool x){return (int)   (Convert.ToInt32(x));}
private static long  btoi_bool_i64(bool x){return (long)  (Convert.ToInt32(x));}

private static bool itob_i8_bool (sbyte x){return x != 0;}
private static bool itob_i16_bool(short x){return x != 0;}
private static bool itob_i32_bool(int x)  {return x != 0;}
private static bool itob_i64_bool(long x) {return x != 0;}

private static sbyte zext_i8_i8(sbyte x)   {return (sbyte) ((byte)(x));}
private static short zext_i8_i16(sbyte x)  {return (short)((byte)(x));}
private static int   zext_i8_i32(sbyte x)  {return (int)((byte)(x));}
private static long  zext_i8_i64(sbyte x)  {return (long)((byte)(x));}

private static sbyte zext_i16_i8(short x)  {return (sbyte) ((ushort)(x));}
private static short zext_i16_i16(short x) {return (short)((ushort)(x));}
private static int   zext_i16_i32(short x) {return (int)((ushort)(x));}
private static long  zext_i16_i64(short x) {return (long)((ushort)(x));}

private static sbyte zext_i32_i8(int x){return (sbyte) ((uint)(x));}
private static short zext_i32_i16(int x){return (short)((uint)(x));}
private static int   zext_i32_i32(int x){return (int)((uint)(x));}
private static long  zext_i32_i64(int x){return (long)((uint)(x));}

private static sbyte zext_i64_i8(long x){return (sbyte) ((ulong)(x));}
private static short zext_i64_i16(long x){return (short)((ulong)(x));}
private static int   zext_i64_i32(long x){return (int)((ulong)(x));}
private static long  zext_i64_i64(long x){return (long)((ulong)(x));}

private static sbyte ssignum(sbyte x){return (sbyte) Math.Sign(x);}
private static short ssignum(short x){return (short) Math.Sign(x);}
private static int ssignum(int x){return Math.Sign(x);}
private static long ssignum(long x){return (long) Math.Sign(x);}

private static sbyte usignum(sbyte x){return ((byte) x > 0) ? (sbyte) 1 : (sbyte) 0;}
private static short usignum(short x){return ((ushort) x > 0) ? (short) 1 : (short) 0;}
private static int usignum(int x){return ((uint) x > 0) ? (int) 1 : (int) 0;}
private static long usignum(long x){return ((ulong) x > 0) ? (long) 1 : (long) 0;}

private static float sitofp_i8_f32(sbyte x){return Convert.ToSingle(x);}
private static float sitofp_i16_f32(short x){return Convert.ToSingle(x);}
private static float sitofp_i32_f32(int x){return Convert.ToSingle(x);}
private static float sitofp_i64_f32(long x){return Convert.ToSingle(x);}

private static double sitofp_i8_f64(sbyte x){return Convert.ToDouble(x);}
private static double sitofp_i16_f64(short x){return Convert.ToDouble(x);}
private static double sitofp_i32_f64(int x){return Convert.ToDouble(x);}
private static double sitofp_i64_f64(long x){return Convert.ToDouble(x);}


private static float uitofp_i8_f32(sbyte x){return Convert.ToSingle(unsigned(x));}
private static float uitofp_i16_f32(short x){return Convert.ToSingle(unsigned(x));}
private static float uitofp_i32_f32(int x){return Convert.ToSingle(unsigned(x));}
private static float uitofp_i64_f32(long x){return Convert.ToSingle(unsigned(x));}

private static double uitofp_i8_f64(sbyte x){return Convert.ToDouble(unsigned(x));}
private static double uitofp_i16_f64(short x){return Convert.ToDouble(unsigned(x));}
private static double uitofp_i32_f64(int x){return Convert.ToDouble(unsigned(x));}
private static double uitofp_i64_f64(long x){return Convert.ToDouble(unsigned(x));}

private static byte fptoui_f32_i8(float x){return (byte) (Math.Truncate(x));}
private static byte fptoui_f64_i8(double x){return (byte) (Math.Truncate(x));}
private static sbyte fptosi_f32_i8(float x){return (sbyte) (Math.Truncate(x));}
private static sbyte fptosi_f64_i8(double x){return (sbyte) (Math.Truncate(x));}

private static ushort fptoui_f32_i16(float x){return (ushort) (Math.Truncate(x));}
private static ushort fptoui_f64_i16(double x){return (ushort) (Math.Truncate(x));}
private static short fptosi_f32_i16(float x){return (short) (Math.Truncate(x));}
private static short fptosi_f64_i16(double x){return (short) (Math.Truncate(x));}

private static uint fptoui_f32_i32(float x){return (uint) (Math.Truncate(x));}
private static uint fptoui_f64_i32(double x){return (uint) (Math.Truncate(x));}
private static int fptosi_f32_i32(float x){return (int) (Math.Truncate(x));}
private static int fptosi_f64_i32(double x){return (int) (Math.Truncate(x));}

private static ulong fptoui_f32_i64(float x){return (ulong) (Math.Truncate(x));}
private static ulong fptoui_f64_i64(double x){return (ulong) (Math.Truncate(x));}
private static long fptosi_f32_i64(float x){return (long) (Math.Truncate(x));}
private static long fptosi_f64_i64(double x){return (long) (Math.Truncate(x));}

private static double fpconv_f32_f64(float x){return Convert.ToDouble(x);}
private static float fpconv_f64_f32(double x){return Convert.ToSingle(x);}

private static double futhark_log64(double x){return Math.Log(x);}
private static double futhark_log2_64(double x){return Math.Log(x,2.0);}
private static double futhark_log10_64(double x){return Math.Log10(x);}
private static double futhark_sqrt64(double x){return Math.Sqrt(x);}
private static double futhark_exp64(double x){return Math.Exp(x);}
private static double futhark_cos64(double x){return Math.Cos(x);}
private static double futhark_sin64(double x){return Math.Sin(x);}
private static double futhark_tan64(double x){return Math.Tan(x);}
private static double futhark_acos64(double x){return Math.Acos(x);}
private static double futhark_asin64(double x){return Math.Asin(x);}
private static double futhark_atan64(double x){return Math.Atan(x);}
private static double futhark_atan2_64(double x, double y){return Math.Atan2(x, y);}
private static double futhark_gamma64(double x){throw new NotImplementedException();}
private static double futhark_lgamma64(double x){throw new NotImplementedException();}
private static bool futhark_isnan64(double x){return double.IsNaN(x);}
private static bool futhark_isinf64(double x){return double.IsInfinity(x);}
private static long futhark_to_bits64(double x){return BitConverter.ToInt64(BitConverter.GetBytes(x),0);}
private static double futhark_from_bits64(long x){return BitConverter.ToDouble(BitConverter.GetBytes(x),0);}

private static float futhark_log32(float x){return (float) Math.Log(x);}
private static float futhark_log2_32(float x){return (float) Math.Log(x,2.0);}
private static float futhark_log10_32(float x){return (float) Math.Log10(x);}
private static float futhark_sqrt32(float x){return (float) Math.Sqrt(x);}
private static float futhark_exp32(float x){return (float) Math.Exp(x);}
private static float futhark_cos32(float x){return (float) Math.Cos(x);}
private static float futhark_sin32(float x){return (float) Math.Sin(x);}
private static float futhark_tan32(float x){return (float) Math.Tan(x);}
private static float futhark_acos32(float x){return (float) Math.Acos(x);}
private static float futhark_asin32(float x){return (float) Math.Asin(x);}
private static float futhark_atan32(float x){return (float) Math.Atan(x);}
private static float futhark_atan2_32(float x, float y){return (float) Math.Atan2(x, y);}
private static float futhark_gamma32(float x){throw new NotImplementedException();}
private static float futhark_lgamma32(float x){throw new NotImplementedException();}
private static bool futhark_isnan32(float x){return float.IsNaN(x);}
private static bool futhark_isinf32(float x){return float.IsInfinity(x);}
private static int futhark_to_bits32(float x){return BitConverter.ToInt32(BitConverter.GetBytes(x), 0);}
private static float futhark_from_bits32(int x){return BitConverter.ToSingle(BitConverter.GetBytes(x), 0);}

private static float futhark_round32(float x){return (float) Math.Round(x);}
private static double futhark_round64(double x){return Math.Round(x);}
private static float futhark_ceil32(float x){return (float) Math.Ceiling(x);}
private static double futhark_ceil64(double x){return Math.Ceiling(x);}
private static float futhark_floor32(float x){return (float) Math.Floor(x);}
private static double futhark_floor64(double x){return Math.Floor(x);}

private static float futhark_lerp32(float v0, float v1, float t){return v0 + (v1-v0)*t;}
private static double futhark_lerp64(double v0, double v1, double t){return v0 + (v1-v0)*t;}

private static bool llt (bool x, bool y){return (!x && y);}
private static bool lle (bool x, bool y){return (!x || y);}

