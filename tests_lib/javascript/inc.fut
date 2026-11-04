-- A small library, for testing initialisation, the utility methods, errors,
-- and the representation of scalar arguments and results.

entry inc (x: i32) : i32 = x + 1

entry fail (x: i32) : i32 = assert (x > 0) x

-- Each primitive type, returned unchanged.
entry id_i8 (x: i8) : i8 = x
entry id_i16 (x: i16) : i16 = x
entry id_i32 (x: i32) : i32 = x
entry id_i64 (x: i64) : i64 = x
entry id_u8 (x: u8) : u8 = x
entry id_u16 (x: u16) : u16 = x
entry id_u32 (x: u32) : u32 = x
entry id_u64 (x: u64) : u64 = x
entry id_f16 (x: f16) : f16 = x
entry id_f32 (x: f32) : f32 = x
entry id_f64 (x: f64) : f64 = x
entry id_bool (x: bool) : bool = x

-- Computed in Futhark, to check that f16 values are not just passed through.
entry f16_of_f64 (x: f64) : f16 = f16.f64 x

-- Returns 'x + 2', unless the tuning parameter 'tunable_x' is set.
entry tunable (x: i64) : i64 = #[param(tunable_x)] x + 2
