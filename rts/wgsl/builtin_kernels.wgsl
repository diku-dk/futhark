// Start of builtin_kernels.wgsl

// Constants used for transpositions. In principle these should be configurable.
//const TR_BLOCK_DIM:        i32 = 16;
//const TR_TILE_DIM:         i32 = 32;
//const TR_ELEMS_PER_THREAD: i32 = 8;

struct MapTransposeParameters {
    dst_mem: i64,       // 0
    dst_offset: i64,    // 8
    src_mem: i64,       // 16
    src_offset: i64,    // 24
    num_arrays: i32,    // 32
    x_elems: i32,       // 36
    y_elems: i32,       // 40
    mulx: i32,          // 44
    muly: i32,          // 48
    repeat_1: i32,      // 52
    repeat_2: i32       // 56
}

@group(0) @binding(0) var<uniform> args: MapTransposeParameters;
// @group(0) @binding(1) var<storage, read_write> shared_mem: array<atomic<i32>>;

// TR_BLOCK_DIM * 2, TR_TILE_DIM / TR_ELEMS_PER_THREAD, 1
@compute @workgroup_size(32, 4, 1)
fn map_transpose_4b(
    @builtin(workgroup_id)         group_id: vec3<u32>,  // tblock_id -> unique id of a group  within a dispatch
    @builtin(local_invocation_id)  local_id: vec3<u32>,  // local_id  -> unique id of a thread within a group
    @builtin(global_invocation_id) global_id: vec3<u32>, // global_id -> unique id of a thread within a dispatch
    @builtin(num_workgroups)       num_groups: vec3<u32>
) {
    return;
}

// End of builtin_kernels.wgsl
