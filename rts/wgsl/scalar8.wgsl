// Start of scalar8.wgsl

alias i8 = i32;

fn read_i8(buffer: ptr<storage, array<i8>, read_write>, i: i32) -> i8 {
  let elem_idx = i / 4;
  let idx_in_elem = i % 4;

  return sext_i8_i32((*buffer)[elem_idx] >> bitcast<u32>(idx_in_elem * 8));
}

fn write_i8(buffer: ptr<storage, array<i8>, read_write>, i: i32, val: i8) {
  let elem_idx = i / 4;
  let idx_in_elem = i % 4;

  let prev = (*buffer)[elem_idx];
  let shift_amt = bitcast<u32>((4 - idx_in_elem) * 8);
  let masked_prev = prev & ~(0xff << shift_amt);
  let shifted_val = (val << shift_amt) & (0xff << shift_amt);

  (*buffer)[elem_idx] = masked_prev | shifted_val;
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
  if (a & 0x80) != 0 { return a | bitcast<i32>(0xffffff00u); }
  return a;
}

// End of scalar8.wgsl
