// Start of atomics.wgsl
//
// Atomic operations on arrays of atomic words, which hold one element of a
// 32-bit type, or several elements of a smaller type (packed from the least
// significant bits).  Each function takes a pointer to the word and the
// position of the element within it.  Only the versions for storage buffers
// are written here; those for workgroup memory ('_shared') are derived from
// them by substitution (see Futhark.CodeGen.RTS.WGSL).

//// atomic read and writes ////

fn atomic_read_i8_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32) -> i8 {
    return unpack_i8(atomicLoad(p), offset);
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

fn atomic_read_bool_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32) -> bool {
    return unpack_bool(atomicLoad(p), offset);
}

fn atomic_write_bool_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, val: bool) {
    if val {
        atomic_write_i8_global(p, offset, 1);
    } else {
        atomic_write_i8_global(p, offset, 0);
    }
}

fn atomic_read_i16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32) -> i16 {
    return unpack_i16(atomicLoad(p), offset);
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

fn atomic_read_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32) -> i32 {
    return unpack_i32(atomicLoad(p), offset);
}

fn atomic_write_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, val: i32) {
    atomicStore(p, val);
}

// A strong compare-and-exchange: atomicCompareExchangeWeak may fail
// spuriously, in which case the old value equals 'cmp' even though nothing was
// written, so callers would mistake the failure for success.
fn atomic_cmp_xchg_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, cmp: i32, x: i32) -> i32 {
    loop {
        let r = atomicCompareExchangeWeak(p, cmp, x);
        if (r.exchanged || r.old_value != cmp) {
            return r.old_value;
        }
    }
}

fn atomic_xchg_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i32) -> i32 {
    return atomicExchange(p, x);
}

//// f32 atomics ////

fn atomic_read_f32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32) -> f32 {
    return unpack_f32(atomicLoad(p), offset);
}

fn atomic_write_f32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, val: f32) {
    atomicStore(p, bitcast<i32>(val));
}

fn atomic_fadd_f32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: f32) -> f32 {
    // Repeatedly take the value out of the slot (leaving zero), add our
    // pending contribution, and put the sum back; whatever was added to the
    // slot in the meantime becomes the next pending contribution.  The
    // termination check is on the bits, because WGSL does not guarantee IEEE
    // semantics for NaN, and a pending NaN must not be dropped.
    var old: i32 = bitcast<i32>(x);
    var ret: f32;

    while (old != 0) {
        ret = bitcast<f32>(atomicExchange(p, 0)) + bitcast<f32>(old);
        old = atomicExchange(p, bitcast<i32>(ret));
    }

    return ret;
}

/// f16 atomics ///

fn atomic_read_f16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32) -> f16 {
    return unpack_f16(atomicLoad(p), offset);
}

fn atomic_write_f16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, val: f16) {
    var x = bitcast<vec2<f16>>(atomicLoad(p));
    var y = x; y[offset] = val;
    while (!atomicCompareExchangeWeak(p, bitcast<i32>(x), bitcast<i32>(y)).exchanged) {
        x = bitcast<vec2<f16>>(atomicLoad(p));
        y = x;
        y[offset] = val;
    }
}

fn atomic_fadd_f16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: f16) -> f16 {
    var val = vec2<f16>(0.0); val[offset] = x;
    var old = bitcast<vec2<f16>>(atomicLoad(p));
    while (!atomicCompareExchangeWeak(p, bitcast<i32>(old), bitcast<i32>(old + val)).exchanged) {
        old = bitcast<vec2<f16>>(atomicLoad(p));
    }

    return old[offset];
}

//// i32 atomics ////

fn atomic_add_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i32) -> i32 {
    return atomicAdd(p, x);
}

fn atomic_smax_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i32) -> i32 {
    return atomicMax(p, x);
}

fn atomic_smin_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i32) -> i32 {
    return atomicMin(p, x);
}

fn atomic_umax_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i32) -> i32 {
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

fn atomic_and_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i32) -> i32 {
    return atomicAnd(p, x);
}

fn atomic_or_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i32) -> i32 {
    return atomicOr(p, x);
}

fn atomic_xor_i32_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i32) -> i32 {
    return atomicXor(p, x);
}

//// i16 atomics ////

fn atomic_cmp_xchg_i16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, cmp: i16, x: i16) -> i16 {
    let shift = bitcast<u32>(offset * 16);
    let mask = 0xffff << shift;
    loop {
        let old = atomicLoad(p);
        let old_i16 = norm_i16(old >> shift);
        if (old_i16 != norm_i16(cmp)) {
            return old_i16;
        }
        if (atomicCompareExchangeWeak(p, old, (old & ~mask) | ((x << shift) & mask)).exchanged) {
            return old_i16;
        }
    }
}

fn atomic_xchg_i16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i16) -> i16 {
    let shift = bitcast<u32>(offset * 16);
    let mask = 0xffff << shift;
    loop {
        let old = atomicLoad(p);
        if (atomicCompareExchangeWeak(p, old, (old & ~mask) | ((x << shift) & mask)).exchanged) {
            return norm_i16(old >> shift);
        }
    }
}

fn atomic_add_i16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i16) -> i16 {
    loop {
        let old = atomicLoad(p);
        let old_i16 = norm_i16(old >> bitcast<u32>(offset * 16));
        let val = norm_u16(add_i16(old_i16, norm_i16(x))) << bitcast<u32>(offset * 16);
        let rest = old & ~(0xffff << bitcast<u32>(offset * 16));

        if (atomicCompareExchangeWeak(p, old, val | rest).exchanged) {
            return old_i16;
        }
    }
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

fn atomic_umax_i16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i16) -> i16 {
    loop {
        let old = atomicLoad(p);
        let old_u16 = norm_u16(old >> bitcast<u32>(offset * 16));
        let val = norm_u16(umax_i16(old_u16, norm_u16(x))) << bitcast<u32>(offset * 16);
        let rest = old & ~(0xffff << bitcast<u32>(offset * 16));

        if (atomicCompareExchangeWeak(p, old, val | rest).exchanged) {
            return norm_i16(old_u16);
        }
    }
}

fn atomic_umin_i16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i16) -> i16 {
    loop {
        let old = atomicLoad(p);
        let old_u16 = norm_u16(old >> bitcast<u32>(offset * 16));
        let val = norm_u16(umin_i16(old_u16, norm_u16(x))) << bitcast<u32>(offset * 16);
        let rest = old & ~(0xffff << bitcast<u32>(offset * 16));

        if (atomicCompareExchangeWeak(p, old, val | rest).exchanged) {
            return norm_i16(old_u16);
        }
    }
}

fn atomic_and_i16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i16) -> i16 {
    let shift = bitcast<u32>(offset * 16);
    let mask = 0xffff << shift;
    return norm_i16(atomicAnd(p, ~mask | (norm_u16(x) << shift)) >> shift);
}

fn atomic_or_i16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i16) -> i16 {
    return norm_i16(atomicOr(p, norm_u16(x) << bitcast<u32>(offset * 16)) >> bitcast<u32>(offset * 16));
}

fn atomic_xor_i16_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i16) -> i16 {
    return norm_i16(atomicXor(p, norm_u16(x) << bitcast<u32>(offset * 16)) >> bitcast<u32>(offset * 16));
}

//// i8 atomics ////

fn atomic_cmp_xchg_i8_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, cmp: i8, x: i8) -> i8 {
    let shift = bitcast<u32>(offset * 8);
    let mask = 0xff << shift;
    loop {
        let old = atomicLoad(p);
        let old_i8 = norm_i8(old >> shift);
        if (old_i8 != norm_i8(cmp)) {
            return old_i8;
        }
        if (atomicCompareExchangeWeak(p, old, (old & ~mask) | ((x << shift) & mask)).exchanged) {
            return old_i8;
        }
    }
}

fn atomic_xchg_i8_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i8) -> i8 {
    let shift = bitcast<u32>(offset * 8);
    let mask = 0xff << shift;
    loop {
        let old = atomicLoad(p);
        if (atomicCompareExchangeWeak(p, old, (old & ~mask) | ((x << shift) & mask)).exchanged) {
            return norm_i8(old >> shift);
        }
    }
}

fn atomic_add_i8_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i8) -> i8 {
    loop {
        let old = atomicLoad(p);
        let old_i8 = norm_i8(old >> bitcast<u32>(offset * 8));
        let val = norm_u8(add_i8(old_i8, norm_i8(x))) << bitcast<u32>(offset * 8);
        let rest = old & ~(0xff << bitcast<u32>(offset * 8));

        if (atomicCompareExchangeWeak(p, old, val | rest).exchanged) {
            return old_i8;
        }
    }
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

fn atomic_umax_i8_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i8) -> i8 {
    loop {
        let old = atomicLoad(p);
        let old_u8 = norm_u8(old >> bitcast<u32>(offset * 8));
        let val = norm_u8(umax_i8(old_u8, norm_u8(x))) << bitcast<u32>(offset * 8);
        let rest = old & ~(0xff << bitcast<u32>(offset * 8));

        if (atomicCompareExchangeWeak(p, old, val | rest).exchanged) {
            return norm_i8(old_u8);
        }
    }
}

fn atomic_umin_i8_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i8) -> i8 {
    loop {
        let old = atomicLoad(p);
        let old_u8 = norm_u8(old >> bitcast<u32>(offset * 8));
        let val = norm_u8(umin_i8(old_u8, norm_u8(x))) << bitcast<u32>(offset * 8);
        let rest = old & ~(0xff << bitcast<u32>(offset * 8));

        if (atomicCompareExchangeWeak(p, old, val | rest).exchanged) {
            return norm_i8(old_u8);
        }
    }
}

fn atomic_and_i8_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i8) -> i8 {
    let shift = bitcast<u32>(offset * 8);
    let mask = 0xff << shift;
    return norm_i8(atomicAnd(p, ~mask | (norm_u8(x) << shift)) >> shift);
}

fn atomic_or_i8_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i8) -> i8 {
    return norm_i8(atomicOr(p, norm_u8(x) << bitcast<u32>(offset * 8)) >> bitcast<u32>(offset * 8));
}

fn atomic_xor_i8_global(p: ptr<storage, atomic<i32>, read_write>, offset: i32, x: i8) -> i8 {
    return norm_i8(atomicXor(p, norm_u8(x) << bitcast<u32>(offset * 8)) >> bitcast<u32>(offset * 8));
}

// End of atomics.wgsl
