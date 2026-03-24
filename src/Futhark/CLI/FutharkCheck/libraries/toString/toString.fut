def digit_to_u8 (d: u64) : u8 =
  '0' + u8.u64 d

def pos_u64_to_string (x: u64) : []u8 =
  if x == 0u64 then
    "0"
  else
    let (_, acc) =
      loop (n, acc) = (x, "")
      while n > 0u64 do
        let digit = n % 10u64
        let ch = digit_to_u8 digit
        in (n / 10u64, [ch] ++ acc)
    in acc

-- Unsigned integers.

def u8_to_string (x: u8) : []u8 =
  pos_u64_to_string (u64.u8 x)

def u16_to_string (x: u16) : []u8 =
  pos_u64_to_string (u64.u16 x)

def u32_to_string (x: u32) : []u8 =
  pos_u64_to_string (u64.u32 x)

def u64_to_string (x: u64) : []u8 =
  pos_u64_to_string x

-- Signed integers.

def i8_to_string (x: i8) : []u8 =
  if x >= 0i8 then
    pos_u64_to_string (u64.i8 x)
  else if x == -128i8 then
    "-128"
  else
    "-" ++ pos_u64_to_string (u64.i8 (-x))

def i16_to_string (x: i16) : []u8 =
  if x >= 0i16 then
    pos_u64_to_string (u64.i16 x)
  else if x == -32768i16 then
    "-32768"
  else
    "-" ++ pos_u64_to_string (u64.i16 (-x))

def i32_to_string (x: i32) : []u8 =
  if x >= 0i32 then
    pos_u64_to_string (u64.i32 x)
  else if x == -2147483648i32 then
    "-2147483648"
  else
    "-" ++ pos_u64_to_string (u64.i32 (-x))

def i64_to_string (x: i64) : []u8 =
  if x >= 0i64 then
    pos_u64_to_string (u64.i64 x)
  else if x == -9223372036854775808i64 then
    "-9223372036854775808"
  else
    "-" ++ pos_u64_to_string (u64.i64 (-x))