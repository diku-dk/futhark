// Start of atomics.wgsl

//// atomic read and writes ////

fn atomic_read_i8_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32) -> i8 {
    let v: i32 = atomicLoad(p);
    return norm_i8(v >> bitcast<u32>(offset * 8));
}

fn atomic_read_i8_shared(p: ptr<workgroup, atomic<i32>>) -> i8 {
    return norm_i8(atomicLoad(p));
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
    atomicStore(p, norm_u8(x));
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
    return norm_i16(atomicLoad(p));
}

fn atomic_write_i16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, val: i16) {
    let shift_amt = bitcast<u32>(offset * 16);

    let mask = 0xffff << shift_amt;
    let shifted_val = (val << shift_amt) & mask;

    // Note: Despite relaxed semantics, this CAS loop is safe, since we are still
    //       sequentially consistent since all ops are operating on the same address.
    var x = atomicLoad(p);
    while (!atomicCompareExchangeWeak(p, x, (x & ~mask) | shifted_val).exchanged) {
        x = atomicLoad(p);
    }
}

fn atomic_write_i16_shared(p: ptr<workgroup, atomic<i32>>, x: i16) {
    atomicStore(p, norm_u16(x));
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

//// f32 atomics ////

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

fn atomic_fadd_f32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: f32) -> f32 {
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

/// f16 atomics ///

// TODO: Should just be packing f16's in shared memory as well
fn atomic_read_f16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32) -> f16 {
    return bitcast<vec2<f16>>(atomicLoad(p))[offset];
}

fn atomic_read_f16_shared(p: ptr<workgroup, atomic<i32>>) -> f16 {
    return bitcast<vec2<f16>>(atomicLoad(p))[0];
}

fn atomic_write_f16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, val: f16) {
    var x = bitcast<vec2<f16>>(atomicLoad(p));
    var y = x; y[offset] = val;
    while (!atomicCompareExchangeWeak(p, bitcast<i32>(x), bitcast<i32>(y)).exchanged) {
        x = bitcast<vec2<f16>>(atomicLoad(p));
        y = x; y[offset] = val;;
    }
}

fn atomic_write_f16_shared(p: ptr<workgroup, atomic<i32>>, x: f16) {
    atomicStore(p, bitcast<i32>(vec2<f16>(x)));
}

fn atomic_fadd_f16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: f16) -> f16 {
    var val = vec2<f16>(0.0); val[offset] = x;
    var old = bitcast<vec2<f16>>(atomicLoad(p));
    while (!atomicCompareExchangeWeak(p, bitcast<i32>(old), bitcast<i32>(old + val)).exchanged) {
        old = bitcast<vec2<f16>>(atomicLoad(p));
    }

    return old[offset];
}

fn atomic_fadd_f16_shared(p: ptr<workgroup, atomic<i32>>, x: f16) -> f16 {
    var val = vec2<f16>(0.0); val[0] = x;
    var old = bitcast<vec2<f16>>(atomicLoad(p));
    while (!atomicCompareExchangeWeak(p, bitcast<i32>(old), bitcast<i32>(old + val)).exchanged) {
        old = bitcast<vec2<f16>>(atomicLoad(p));
    }

    return old[0];
}

//// i32 atomics ////

fn atomic_add_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i32) -> i32 {
    return atomicAdd(p, x);
}

fn atomic_add_i32_shared(p: ptr<workgroup, atomic<i32>>, x: i32) -> i32 {
    return atomicAdd(p, x);
}

fn atomic_smax_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i32) -> i32 {
    return atomicMax(p, x);
}

fn atomic_smax_i32_shared(p: ptr<workgroup, atomic<i32>>, x: i32) -> i32 {
    return atomicMax(p, x);
}

fn atomic_smin_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i32) -> i32 {
    return atomicMin(p, x);
}

fn atomic_smin_i32_shared(p: ptr<workgroup, atomic<i32>>, x: i32) -> i32 {
    return atomicMin(p, x);
}

fn atomic_umax_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i32) -> i32 {
    var old = atomicLoad(p);
    while (!atomicCompareExchangeWeak(p, old, umax_i32(old, x)).exchanged) {
        old = atomicLoad(p);
    }

    return old;
}

fn atomic_umax_i32_shared(p: ptr<workgroup, atomic<i32>>, x: i32) -> i32 {
    var old = atomicLoad(p);
    while (!atomicCompareExchangeWeak(p, old, umax_i32(old, x)).exchanged) {
        old = atomicLoad(p);
    }

    return old;
}

fn atomic_umin_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i32) -> i32 {
    var old = atomicLoad(p);
    while (!atomicCompareExchangeWeak(p, old, umin_i32(old, x)).exchanged) {
        old = atomicLoad(p);
    }

    return old;
}

fn atomic_umin_i32_shared(p: ptr<workgroup, atomic<i32>>, x: i32) -> i32 {
    var old = atomicLoad(p);
    while (!atomicCompareExchangeWeak(p, old, umin_i32(old, x)).exchanged) {
        old = atomicLoad(p);
    }

    return old;
}

fn atomic_and_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i32) -> i32 {
    return atomicAnd(p, x);
}

fn atomic_and_i32_shared(p: ptr<workgroup, atomic<i32>>, x: i32) -> i32 {
    return atomicAnd(p, x);
}

fn atomic_or_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i32) -> i32 {
    return atomicOr(p, x);
}

fn atomic_or_i32_shared(p: ptr<workgroup, atomic<i32>>, x: i32) -> i32 {
    return atomicOr(p, x);
}

fn atomic_xor_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i32) -> i32 {
    return atomicXor(p, x);
}

fn atomic_xor_i32_shared(p: ptr<workgroup, atomic<i32>>, x: i32) -> i32 {
    return atomicXor(p, x);
}

//// i16 atomics ////

fn atomic_add_i16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i16) -> i16 {
    loop {
        let old = atomicLoad(p);
        let old_i16 = norm_i16(old >> bitcast<u32>(offset * 16));
        let val = i32(add_i16(old_i16, norm_i16(x))) << bitcast<u32>(offset * 16);
        let rest = old & ~(0xffff << bitcast<u32>(offset * 16));

        if (atomicCompareExchangeWeak(p, old, val | rest).exchanged) {
            return old_i16;
        }
    }
}

fn atomic_add_i16_shared(p: ptr<workgroup, atomic<i32>>, x: i16) -> i16 {
    var old = atomicLoad(p);
    while (!atomicCompareExchangeWeak(p, old, add_i16(norm_i16(old), norm_i16(x))).exchanged) {
        old = atomicLoad(p);
    }

    return norm_i16(old);
}

fn atomic_smax_i16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i16) -> i16 {
    loop {
        let old = atomicLoad(p);
        let old_i16 = norm_i16(old >> bitcast<u32>(offset * 16));
        let val = norm_u16(max(old_i16, norm_i16(x))) << bitcast<u32>(offset * 16);
        let rest = old & ~(0xffff << bitcast<u32>(offset * 16));

        if (atomicCompareExchangeWeak(p, old, val | rest).exchanged) {
            return old_i16;
        }
    }
}

fn atomic_smax_i16_shared(p: ptr<workgroup, atomic<i32>>, x: i16) -> i16 {
    var old = atomicLoad(p);
    while (!atomicCompareExchangeWeak(p, old, max(norm_i16(old), norm_i16(x))).exchanged) {
        old = atomicLoad(p);
    }

    return norm_i16(old);
}

fn atomic_smin_i16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i16) -> i16 {
    loop {
        let old = atomicLoad(p);
        let old_i16 = norm_i16(old >> bitcast<u32>(offset * 16));
        let val = norm_u16(min(old_i16, norm_i16(x))) << bitcast<u32>(offset * 16);
        let rest = old & ~(0xffff << bitcast<u32>(offset * 16));

        if (atomicCompareExchangeWeak(p, old, val | rest).exchanged) {
            return old_i16;
        }
    }
}

fn atomic_smin_i16_shared(p: ptr<workgroup, atomic<i32>>, x: i16) -> i16 {
    var old = atomicLoad(p);
    while (!atomicCompareExchangeWeak(p, old, min(norm_i16(old), norm_i16(x))).exchanged) {
        old = atomicLoad(p);
    }

    return norm_i16(old);
}

fn atomic_umax_i16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i16) -> i16 {
    loop {
        let old = atomicLoad(p);
        let old_u16 = norm_u16(old >> bitcast<u32>(offset * 16));
        let val = norm_u16(umax_i16(old_u16, norm_u16(x))) << bitcast<u32>(offset * 16);
        let rest = old & ~(0xffff << bitcast<u32>(offset * 16));

        if (atomicCompareExchangeWeak(p, old, val | rest).exchanged) {
            return old_u16;
        }
    }
}

fn atomic_umax_i16_shared(p: ptr<workgroup, atomic<i32>>, x: i16) -> i16 {
    var old = atomicLoad(p);
    while (!atomicCompareExchangeWeak(p, old, umax_i16(norm_u16(old), norm_u16(x))).exchanged) {
        old = atomicLoad(p);
    }

    return norm_u16(old);
}

fn atomic_umin_i16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i16) -> i16 {
    loop {
        let old = atomicLoad(p);
        let old_u16 = norm_u16(old >> bitcast<u32>(offset * 16));
        let val = norm_u16(umin_i16(old_u16, norm_u16(x))) << bitcast<u32>(offset * 16);
        let rest = old & ~(0xffff << bitcast<u32>(offset * 16));

        if (atomicCompareExchangeWeak(p, old, val | rest).exchanged) {
            return old_u16;
        }
    }
}

fn atomic_umin_i16_shared(p: ptr<workgroup, atomic<i32>>, x: i16) -> i16 {
    var old = atomicLoad(p);
    while (!atomicCompareExchangeWeak(p, old, umin_i16(norm_u16(old), norm_u16(x))).exchanged) {
        old = atomicLoad(p);
    }

    return norm_u16(old);
}

fn atomic_and_i16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i16) -> i16 {
    return norm_u16(atomicAnd(p, norm_u16(x) << bitcast<u32>(offset * 16)) >> bitcast<u32>(offset * 16));
}

fn atomic_and_i16_shared(p: ptr<workgroup, atomic<i32>>, x: i16) -> i16 {
    return norm_u16(atomicAnd(p, norm_u16(x)));
}

fn atomic_or_i16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i16) -> i16 {
    return norm_u16(atomicOr(p, norm_u16(x) << bitcast<u32>(offset * 16)) >> bitcast<u32>(offset * 16));
}

fn atomic_or_i16_shared(p: ptr<workgroup, atomic<i32>>, x: i16) -> i16 {
    return norm_u16(atomicOr(p, norm_u16(x)));
}

fn atomic_xor_i16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i16) -> i16 {
    return norm_u16(atomicXor(p, norm_u16(x) << bitcast<u32>(offset * 16)) >> bitcast<u32>(offset * 16));
}

fn atomic_xor_i16_shared(p: ptr<workgroup, atomic<i32>>, x: i16) -> i16 {
    return norm_u16(atomicXor(p, norm_u16(x)));
}

//// i8 atomics ////

fn atomic_add_i8_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i8) -> i8 {
    loop {
        let old = atomicLoad(p);
        let old_i8 = norm_i8(old >> bitcast<u32>(offset * 8));
        let val = i32(add_i8(old_i8, norm_i8(x))) << bitcast<u32>(offset * 8);
        let rest = old & ~(0xff << bitcast<u32>(offset * 8));

        if (atomicCompareExchangeWeak(p, old, val | rest).exchanged) {
            return old_i8;
        }
    }
}

fn atomic_add_i8_shared(p: ptr<workgroup, atomic<i32>>, x: i8) -> i8 {
    var old = atomicLoad(p);
    while (!atomicCompareExchangeWeak(p, old, add_i8(norm_i8(old), norm_i8(x))).exchanged) {
        old = atomicLoad(p);
    }

    return norm_i8(old);
}

fn atomic_smax_i8_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i8) -> i8 {
    loop {
        let old = atomicLoad(p);
        let old_i8 = norm_i8(old >> bitcast<u32>(offset * 8));
        let val = norm_u8(max(old_i8, norm_i8(x))) << bitcast<u32>(offset * 8);
        let rest = old & ~(0xff << bitcast<u32>(offset * 8));

        if (atomicCompareExchangeWeak(p, old, val | rest).exchanged) {
            return old_i8;
        }
    }
}

fn atomic_smax_i8_shared(p: ptr<workgroup, atomic<i32>>, x: i8) -> i8 {
    var old = atomicLoad(p);
    while (!atomicCompareExchangeWeak(p, old, max(norm_i8(old), norm_i8(x))).exchanged) {
        old = atomicLoad(p);
    }

    return norm_i8(old);
}

fn atomic_smin_i8_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i8) -> i8 {
    loop {
        let old = atomicLoad(p);
        let old_i8 = norm_i8(old >> bitcast<u32>(offset * 8));
        let val = norm_u8(min(old_i8, norm_i8(x))) << bitcast<u32>(offset * 8);
        let rest = old & ~(0xff << bitcast<u32>(offset * 8));

        if (atomicCompareExchangeWeak(p, old, val | rest).exchanged) {
            return old_i8;
        }
    }
}

fn atomic_smin_i8_shared(p: ptr<workgroup, atomic<i32>>, x: i8) -> i8 {
    var old = atomicLoad(p);
    while (!atomicCompareExchangeWeak(p, old, min(norm_i8(old), norm_i8(x))).exchanged) {
        old = atomicLoad(p);
    }

    return norm_i8(old);
}

fn atomic_umax_i8_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i8) -> i8 {
    loop {
        let old = atomicLoad(p);
        let old_u8 = norm_u8(old >> bitcast<u32>(offset * 8));
        let val = norm_u8(umax_i8(old_u8, norm_u8(x))) << bitcast<u32>(offset * 8);
        let rest = old & ~(0xff << bitcast<u32>(offset * 8));

        if (atomicCompareExchangeWeak(p, old, val | rest).exchanged) {
            return old_u8;
        }
    }
}

fn atomic_umax_i8_shared(p: ptr<workgroup, atomic<i32>>, x: i8) -> i8 {
    var old = atomicLoad(p);
    while (!atomicCompareExchangeWeak(p, old, umax_i8(norm_u8(old), norm_u8(x))).exchanged) {
        old = atomicLoad(p);
    }

    return norm_u8(old);
}

fn atomic_umin_i8_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i8) -> i8 {
    loop {
        let old = atomicLoad(p);
        let old_u8 = norm_u8(old >> bitcast<u32>(offset * 8));
        let val = norm_u8(umin_i8(old_u8, norm_u8(x))) << bitcast<u32>(offset * 8);
        let rest = old & ~(0xff << bitcast<u32>(offset * 8));

        if (atomicCompareExchangeWeak(p, old, val | rest).exchanged) {
            return old_u8;
        }
    }
}

fn atomic_umin_i8_shared(p: ptr<workgroup, atomic<i32>>, x: i8) -> i8 {
    var old = atomicLoad(p);
    while (!atomicCompareExchangeWeak(p, old, umin_i8(norm_u8(old), norm_u8(x))).exchanged) {
        old = atomicLoad(p);
    }

    return norm_u8(old);
}

fn atomic_and_i8_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i8) -> i8 {
    return norm_u8(atomicAnd(p, norm_u8(x) << bitcast<u32>(offset * 8)) >> bitcast<u32>(offset * 8));
}

fn atomic_and_i8_shared(p: ptr<workgroup, atomic<i32>>, x: i8) -> i8 {
    return norm_u8(atomicAnd(p, norm_u8(x)));
}

fn atomic_or_i8_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i8) -> i8 {
    return norm_u8(atomicOr(p, norm_u8(x) << bitcast<u32>(offset * 8)) >> bitcast<u32>(offset * 8));
}

fn atomic_or_i8_shared(p: ptr<workgroup, atomic<i32>>, x: i8) -> i8 {
    return norm_u8(atomicOr(p, norm_u8(x)));
}

fn atomic_xor_i8_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i8) -> i8 {
    return norm_u8(atomicXor(p, norm_u8(x) << bitcast<u32>(offset * 8)) >> bitcast<u32>(offset * 8));
}

fn atomic_xor_i8_shared(p: ptr<workgroup, atomic<i32>>, x: i8) -> i8 {
    return norm_u8(atomicXor(p, norm_u8(x)));
}

// End of atomics.wgsl
