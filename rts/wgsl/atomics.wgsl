// Start of atomics.wgsl

// TODO: implement CAS for i8, i16 atomic ops

fn atomic_read_i8_global(p: ptr<storage, atomic<i32>, read_write>) -> i8 {
    return atomicLoad(p);
}

fn atomic_read_i8_shared(p: ptr<workgroup, atomic<i32>>) -> i8 {
    return atomicLoad(p);
}

fn atomic_write_i8_global(p: ptr<storage, atomic<i32>, read_write>, x: i8) {
    atomicStore(p, x);
}

fn atomic_write_i8_shared(p: ptr<workgroup, atomic<i32>>, x: i8) {
    atomicStore(p, x);
}

fn atomic_read_i16_global(p: ptr<storage, atomic<i32>, read_write>) -> i16 {
    return atomicLoad(p);
}

fn atomic_read_i16_shared(p: ptr<workgroup, atomic<i32>>) -> i16 {
    return atomicLoad(p);
}

fn atomic_write_i16_global(p: ptr<storage, atomic<i32>, read_write>, x: i16) {
    atomicStore(p, x);
}

fn atomic_write_i16_shared(p: ptr<workgroup, atomic<i32>>, x: i16) {
    atomicStore(p, x);
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
