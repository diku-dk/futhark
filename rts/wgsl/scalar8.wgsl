// Start of scalar8.wgsl

alias i8 = i32;

fn read_i8(buffer: ptr<storage, i8, read_write>, i: i32) -> i8 {
  
}

fn add_i8(a: i8, b: i8) -> i8 {
  return sext_i8_i32(a + b);
}

fn neg_i8(a: i8) -> i8 {
  return add_i8(~a, 1);
}

fn sub_i8(a: i8, b: i8) -> i8 {
  return add_i8(a, neg_i8(b));
}

fn mul_i8(a: i8, b: i8) -> i8 {
  return sext_i8_i32(a * b);
}

fn zext_i8_i32(a: i8) -> i32 {
  return a & 0xff;
}

fn sext_i8_i32(a: i8) -> i32 {
  if (a & 0x80) != 0 { return a | 0xffffff00; }
  return a;
}

// End of scalar8.wgsl
