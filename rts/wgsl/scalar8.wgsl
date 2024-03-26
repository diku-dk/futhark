// Start of scalar8.wgsl

alias i8 = i32;

fn norm(a: i8) -> i32 {
  if (a & 0x80) != 0 { return a | bitcast<i32>(0xffffff00u); }
  return a & 0x000000ff;
}


fn read_i8(buffer: ptr<storage, array<atomic<i8>>, read_write>, i: i32) -> i8 {
  let elem_idx = i / 4;
  let idx_in_elem = i % 4;

  // mem: b0b1b2b3
  // int: b3b2b1b0
  let v = atomicLoad(&((*buffer)[elem_idx]));
  return norm(v >> bitcast<u32>(idx_in_elem * 8));
}

fn write_i8(buffer: ptr<storage, array<atomic<i8>>, read_write>,
            i: i32,
			val: i8
) {
  let elem_idx = i / 4;
  let idx_in_elem = i % 4;

  // mem: b0b1b2b3
  // int: b3b2b1b0
  let shift_amt = bitcast<u32>(idx_in_elem * 8);

  let mask = 0xff << shift_amt;
  let shifted_val = (val << shift_amt) & mask;

  // First zero out the previous value using the inverted mask.
  atomicAnd(&((*buffer)[elem_idx]), ~mask);
  // And then write the new value.
  atomicOr(&((*buffer)[elem_idx]), shifted_val);
}

fn add_i8(a: i8, b: i8) -> i8 {
  return norm(a + b);
}

fn neg_i8(a: i8) -> i8 {
  return add_i8(~a, 1);
}

fn sub_i8(a: i8, b: i8) -> i8 {
  return add_i8(a, neg_i8(b));
}

fn mul_i8(a: i8, b: i8) -> i8 {
  return norm(a * b);
}

fn umin_i8(a: i8, b: i8) -> i8 {
  return umin_i32(a, b);
}

fn umax_i8(a: i8, b: i8) -> i8 {
  return umax_i32(a, b);
}

fn shl_i8(a: i8, b: i8) -> i8 {
  return a << bitcast<u32>(b);
}

fn lshr_i8(a: i8, b: i8) -> i8 {
  return bitcast<i8>(bitcast<u32>(a) >> bitcast<u32>(b));
}

fn ashr_i8(a: i8, b: i8) -> i8 {
  return a >> bitcast<u32>(b);
}

fn pow_i8(a_p: i8, b: i8) -> i8 {
  var a = a_p;
  var res: i8 = 1;
  var rem: i8 = b;

  while rem != 0 {
    if (rem & 1) != 0 {
	  res = mul_i8(res, a);
	}
	rem = ashr_i8(rem, 1);
	a = mul_i8(a, a);
  }

  return res;
}

fn zext_i8_i32(a: i8) -> i32 {
  return a & 0xff;
}

// Our representation is already a sign-extended i32.
fn sext_i8_i32(a: i8) -> i32 {
  return a;
}

fn zext_i8_i64(a: i8) -> i64 {
  return i64(zext_i8_i32(a), 0);
}

fn sext_i8_i64(a: i8) -> i64 {
  return sext_i32_i64(a);
}

fn trunc_i32_i8(a: i32) -> i8 {
  return a & 0xff;
}

fn trunc_i64_i8(a: i64) -> i8 {
  return trunc_i32_i8(a.x);
}

// End of scalar8.wgsl
