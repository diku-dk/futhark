// Start of atomics.wgsl

fn atomic_fadd_f32_global(p: ptr<storage, atomic<i32>, read_write>, x: f32) -> f32 {
    var old: f32 = x;
    var ret: f32;

    loop {
        ret = bitcast<f32>(atomicExchange(p, 0)) + old;
        old = bitcast<f32>(atomicExchange(p, bitcast<i32>(ret)));

        if (old == 0) { break; }
    }

    return ret;
}

fn atomic_fadd_f32_shared(p: ptr<workgroup, atomic<i32>>, x: f32) -> f32 {
    var old: f32 = x;
    var ret: f32;

    loop {
        ret = bitcast<f32>(atomicExchange(p, 0)) + old;
        old = bitcast<f32>(atomicExchange(p, bitcast<i32>(ret)));

        if (old == 0) { break; }
    }

    return ret;
}

// TODO: while(old) instead of == 0 check

// End of atomics.wgsl
