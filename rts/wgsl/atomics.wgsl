// Start of atomics.wgsl

fn atomic_read_i8_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32) -> i8 {
    let v: i32 = atomicLoad(p);
    return norm_i8(v >> bitcast<u32>(offset * 8));
}

fn atomic_read_i8_shared(p: ptr<workgroup, atomic<i32>>) -> i8 {
    return atomicLoad(p);
}

fn atomic_write_i8_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, val: i8) {
    let shift_amt = bitcast<u32>(offset * 8);

    let mask = 0xff << shift_amt;
    let shifted_val = (val << shift_amt) & mask;

    // Note: Despite relaxed semantics, this CAS loop is safe, since we are still
    //       sequentially consistent since all ops are operating on the same address.
    var x = atomicLoad(p);
    while (!atomicCompareExchangeWeak(p, x, (x & ~mask) | shifted_val).exchanged) {
        x = atomicLoad(p);
    }
}

fn atomic_write_i8_shared(p: ptr<workgroup, atomic<i32>>, x: i8) {
    atomicStore(p, x);
}

fn atomic_read_bool_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32) -> bool {
    return atomic_read_i8_global(p, offset) == 1;
}

fn atomic_read_bool_shared(p: ptr<workgroup, atomic<i32>>) -> bool {
    return atomic_read_i8_shared(p) == 1;
}

fn atomic_write_bool_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, val: bool) {
    if val {
        atomic_write_i8_global(p, offset, 1);
    } else {
        atomic_write_i8_global(p, offset, 0);
    }
}

fn atomic_write_bool_shared(p: ptr<workgroup, atomic<i32>>, x: bool) {
    if x {
        atomic_write_i8_shared(p, 1);
    } else {
        atomic_write_i8_shared(p, 0);
    }
}

fn atomic_read_i16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32) -> i16 {
    let v: i32 = atomicLoad(p);
    return norm_i16(v >> bitcast<u32>(offset * 16));
}

fn atomic_read_i16_shared(p: ptr<workgroup, atomic<i32>>) -> i16 {
    return atomicLoad(p);
}

fn atomic_write_i16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, val: i16) {
    let shift_amt = bitcast<u32>(offset * 16);

    let mask = 0xffff << shift_amt;
    let shifted_val = val << shift_amt;

    // Note: Despite relaxed semantics, this CAS loop is safe, since we are still
    //       sequentially consistent since all ops are operating on the same address.
    var x = atomicLoad(p);
    while (!atomicCompareExchangeWeak(p, x, (x & ~mask) | shifted_val).exchanged) {
        x = atomicLoad(p);
    }
}

fn atomic_write_i16_shared(p: ptr<workgroup, atomic<i32>>, x: i16) {
    atomicStore(p, x);
}

fn atomic_read_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32) -> i32 {
    return atomicLoad(p);
}

fn atomic_read_i32_shared(p: ptr<workgroup, atomic<i32>>) -> i32 {
    return atomicLoad(p);
}

fn atomic_write_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, val: i32) {
    atomicStore(p, val);
}

fn atomic_write_i32_shared(p: ptr<workgroup, atomic<i32>>, x: i32) {
    atomicStore(p, x);
}

fn atomic_read_f32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32) -> f32 {
    return bitcast<f32>(atomicLoad(p));
}

fn atomic_read_f32_shared(p: ptr<workgroup, atomic<i32>>) -> f32 {
    return bitcast<f32>(atomicLoad(p));
}

fn atomic_write_f32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, val: f32) {
    atomicStore(p, bitcast<i32>(val));
}

fn atomic_write_f32_shared(p: ptr<workgroup, atomic<i32>>, x: f32) {
    atomicStore(p, bitcast<i32>(x));
}

fn atomic_fadd_f32_global(p: ptr<storage, atomic<i32>, read_write>, x: f32) -> f32 {
    var old: f32 = x;
    var ret: f32;

    while (old != 0) {
        ret = bitcast<f32>(atomicExchange(p, 0)) + old;
        old = bitcast<f32>(atomicExchange(p, bitcast<i32>(ret)));
    }

    return ret;
}

fn atomic_fadd_f32_shared(p: ptr<workgroup, atomic<i32>>, x: f32) -> f32 {
    var old: f32 = x;
    var ret: f32;

    while (old != 0) {
        ret = bitcast<f32>(atomicExchange(p, 0)) + old;
        old = bitcast<f32>(atomicExchange(p, bitcast<i32>(ret)));
    }

    return ret;
}

// End of atomics.wgsl
