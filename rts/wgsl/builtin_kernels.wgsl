// Start of builtin_kernels.wgsl

// Constants used for transpositions. In principle these should be configurable.
const TR_BLOCK_DIM:        i32 = 16;
const TR_TILE_DIM:         i32 = 32;
const TR_ELEMS_PER_THREAD: i32 = 8;

override lmad_copy_block_size_x: i32 = 1;
override lmad_copy_block_size_y: i32 = 1;
override lmad_copy_block_size_z: i32 = 1;

override map_transpose_block_size_x: i32 = 1;
override map_transpose_block_size_y: i32 = 1;
override map_transpose_block_size_z: i32 = 1;

override map_transpose_low_height_block_size_x: i32 = 1;
override map_transpose_low_height_block_size_y: i32 = 1;
override map_transpose_low_height_block_size_z: i32 = 1;

override map_transpose_low_width_block_size_x: i32 = 1;
override map_transpose_low_width_block_size_y: i32 = 1;
override map_transpose_low_width_block_size_z: i32 = 1;

override map_transpose_small_block_size_x: i32 = 1;
override map_transpose_small_block_size_y: i32 = 1;
override map_transpose_small_block_size_z: i32 = 1;

override map_transpose_large_block_size_x: i32 = 1;
override map_transpose_large_block_size_y: i32 = 1;
override map_transpose_large_block_size_z: i32 = 1;

struct MapTransposeParameters {
    dst_offset: i64,    // 0
    src_offset: i64,    // 8
    num_arrays: i32,    // 16
    x_elems: i32,       // 20
    y_elems: i32,       // 24
    mulx: i32,          // 28
    muly: i32,          // 32
    repeat_1: i32,      // 36
    repeat_2: i32       // 40
}

struct MapTransposeParametersLarge {
    dst_offset: i64,    // 0
    src_offset: i64,    // 8
    num_arrays: i64,    // 16
    x_elems: i64,       // 24
    y_elems: i64,       // 32
    mulx: i64,          // 40
    muly: i64,          // 48
    repeat_1: i32,      // 56
    repeat_2: i32       // 60
}

struct CopyParameters {
    dst_offset: i64,
    src_offset: i64,
    n: i64,
    r: i32,
    shape0: i64, dst_stride0: i64, src_stride0: i64,
    shape1: i64, dst_stride1: i64, src_stride1: i64,
    shape2: i64, dst_stride2: i64, src_stride2: i64,
    shape3: i64, dst_stride3: i64, src_stride3: i64,
    shape4: i64, dst_stride4: i64, src_stride4: i64,
    shape5: i64, dst_stride5: i64, src_stride5: i64,
    shape6: i64, dst_stride6: i64, src_stride6: i64,
    shape7: i64, dst_stride7: i64, src_stride7: i64
}

// Begin of builtin kernel group: NAME, ELEM_TYPE

@group(0) @binding(0) var<uniform> copy_args_NAME: CopyParameters;
@group(0) @binding(1) var<storage, read_write> copy_dst_NAME_mem: array<ELEM_TYPE>;
@group(0) @binding(2) var<storage, read_write> copy_src_NAME_mem: array<ELEM_TYPE>;
@compute @workgroup_size(lmad_copy_block_size_x, lmad_copy_block_size_y, lmad_copy_block_size_z)
fn lmad_copy_NAME(@builtin(global_invocation_id) global_id: vec3<u32>) {
    var remainder: i32 = i32(global_id.x);
    var dst_offset: i32 = copy_args_NAME.dst_offset.x;
    var src_offset: i32 = copy_args_NAME.src_offset.x;

    if (i32(global_id.x) >= copy_args_NAME.n.x) {
      return;
    }

    if (copy_args_NAME.r > 0) {
      let i: i32 = remainder % copy_args_NAME.shape0.x;
      dst_offset += i * copy_args_NAME.dst_stride0.x;
      src_offset += i * copy_args_NAME.src_stride0.x;
      remainder  /= copy_args_NAME.shape0.x;
    }
    if (copy_args_NAME.r > 1) {
      let i: i32 = remainder % copy_args_NAME.shape1.x;
      dst_offset += i * copy_args_NAME.dst_stride1.x;
      src_offset += i * copy_args_NAME.src_stride1.x;
      remainder  /= copy_args_NAME.shape1.x;
    }
    if (copy_args_NAME.r > 2) {
      let i: i32 = remainder % copy_args_NAME.shape2.x;
      dst_offset += i * copy_args_NAME.dst_stride2.x;
      src_offset += i * copy_args_NAME.src_stride2.x;
      remainder  /= copy_args_NAME.shape2.x;
    }
    if (copy_args_NAME.r > 3) {
      let i: i32 = remainder % copy_args_NAME.shape3.x;
      dst_offset += i * copy_args_NAME.dst_stride3.x;
      src_offset += i * copy_args_NAME.src_stride3.x;
      remainder  /= copy_args_NAME.shape3.x;
    }
    if (copy_args_NAME.r > 4) {
      let i: i32 = remainder % copy_args_NAME.shape4.x;
      dst_offset += i * copy_args_NAME.dst_stride4.x;
      src_offset += i * copy_args_NAME.src_stride4.x;
      remainder  /= copy_args_NAME.shape4.x;
    }
    if (copy_args_NAME.r > 5) {
      let i: i32 = remainder % copy_args_NAME.shape5.x;
      dst_offset += i * copy_args_NAME.dst_stride5.x;
      src_offset += i * copy_args_NAME.src_stride5.x;
      remainder  /= copy_args_NAME.shape5.x;
    }
    if (copy_args_NAME.r > 6) {
      let i: i32 = remainder % copy_args_NAME.shape6.x;
      dst_offset += i * copy_args_NAME.dst_stride6.x;
      src_offset += i * copy_args_NAME.src_stride6.x;
      remainder  /= copy_args_NAME.shape6.x;
    }
    if (copy_args_NAME.r > 7) {
      let i: i32 = remainder % copy_args_NAME.shape7.x;
      dst_offset += i * copy_args_NAME.dst_stride7.x;
      src_offset += i * copy_args_NAME.src_stride7.x;
      remainder  /= copy_args_NAME.shape7.x;
    }

    copy_dst_NAME_mem[dst_offset] = copy_src_NAME_mem[src_offset];
}

@group(0) @binding(0) var<uniform> mt_NAME_args: MapTransposeParameters;
@group(0) @binding(1) var<storage, read_write> mt_NAME_dst_mem: array<ELEM_TYPE>;
@group(0) @binding(2) var<storage, read_write> mt_NAME_src_mem: array<ELEM_TYPE>;
@compute @workgroup_size(map_transpose_block_size_x, map_transpose_block_size_y, map_transpose_block_size_z)
fn map_transpose_NAME(
    @builtin(workgroup_id)         group_id: vec3<u32>,  // tblock_id -> unique id of a group  within a dispatch
    @builtin(global_invocation_id) global_id: vec3<u32>, // global_id -> unique id of a thread within a dispatch
    @builtin(local_invocation_id)  local_id: vec3<u32>,  // local_id  -> unique id of a thread within a group
    @builtin(num_workgroups)       num_groups: vec3<u32>
) {
    for (var y = 0; y < mt_NAME_args.y_elems; y += 1) {
        for (var x = 0; x < mt_NAME_args.x_elems; x += 1) {
            mt_NAME_dst_mem[mt_NAME_args.dst_offset[0] + x * mt_NAME_args.y_elems + y] = 
                mt_NAME_src_mem[mt_NAME_args.src_offset[0] + y * mt_NAME_args.y_elems + x];
        }
    }
}

@group(0) @binding(0) var<uniform> mt_lh_NAME_args: MapTransposeParameters;
@group(0) @binding(1) var<storage, read_write> mt_lh_NAME_dst_mem: array<ELEM_TYPE>;
@group(0) @binding(2) var<storage, read_write> mt_lh_NAME_src_mem: array<ELEM_TYPE>;
@compute @workgroup_size(map_transpose_low_height_block_size_x, map_transpose_low_height_block_size_y, map_transpose_low_height_block_size_z)
fn map_transpose_NAME_low_height(
    @builtin(workgroup_id)         group_id: vec3<u32>,  // tblock_id -> unique id of a group  within a dispatch
    @builtin(global_invocation_id) global_id: vec3<u32>, // global_id -> unique id of a thread within a dispatch
    @builtin(local_invocation_id)  local_id: vec3<u32>,  // local_id  -> unique id of a thread within a group
    @builtin(num_workgroups)       num_groups: vec3<u32>
) {
    for (var y = 0; y < mt_lh_NAME_args.y_elems; y += 1) {
        for (var x = 0; x < mt_lh_NAME_args.x_elems; x += 1) {
            mt_lh_NAME_dst_mem[mt_lh_NAME_args.dst_offset[0] + x * mt_lh_NAME_args.y_elems + y] = 
                mt_lh_NAME_src_mem[mt_lh_NAME_args.src_offset[0] + y * mt_lh_NAME_args.y_elems + x];
        }
    }
}

@group(0) @binding(0) var<uniform> mt_lw_NAME_args: MapTransposeParameters;
@group(0) @binding(1) var<storage, read_write> mt_lw_NAME_dst_mem: array<ELEM_TYPE>;
@group(0) @binding(2) var<storage, read_write> mt_lw_NAME_src_mem: array<ELEM_TYPE>;
@compute @workgroup_size(map_transpose_low_width_block_size_x, map_transpose_low_width_block_size_y, map_transpose_low_width_block_size_z)
fn map_transpose_NAME_low_width(
    @builtin(workgroup_id)         group_id: vec3<u32>,  // tblock_id -> unique id of a group  within a dispatch
    @builtin(global_invocation_id) global_id: vec3<u32>, // global_id -> unique id of a thread within a dispatch
    @builtin(local_invocation_id)  local_id: vec3<u32>,  // local_id  -> unique id of a thread within a group
    @builtin(num_workgroups)       num_groups: vec3<u32>
) {
    for (var y = 0; y < mt_lw_NAME_args.y_elems; y += 1) {
        for (var x = 0; x < mt_lw_NAME_args.x_elems; x += 1) {
            mt_lw_NAME_dst_mem[mt_lw_NAME_args.dst_offset[0] + x * mt_lw_NAME_args.y_elems + y] =
                mt_lw_NAME_src_mem[mt_lw_NAME_args.src_offset[0] + y * mt_lw_NAME_args.y_elems + x];
        }
    }
}

@group(0) @binding(0) var<uniform> mt_s_NAME_args: MapTransposeParameters;
@group(0) @binding(1) var<storage, read_write> mt_s_NAME_dst_mem: array<ELEM_TYPE>;
@group(0) @binding(2) var<storage, read_write> mt_s_NAME_src_mem: array<ELEM_TYPE>;
@compute @workgroup_size(map_transpose_small_block_size_x, map_transpose_small_block_size_y, map_transpose_small_block_size_z)
fn map_transpose_NAME_small(
    @builtin(workgroup_id)         group_id: vec3<u32>,  // tblock_id -> unique id of a group  within a dispatch
    @builtin(global_invocation_id) global_id: vec3<u32>, // global_id -> unique id of a thread within a dispatch
    @builtin(local_invocation_id)  local_id: vec3<u32>,  // local_id  -> unique id of a thread within a group
    @builtin(num_workgroups)       num_groups: vec3<u32>
) {
    for (var y = 0; y < mt_s_NAME_args.y_elems; y += 1) {
        for (var x = 0; x < mt_s_NAME_args.x_elems; x += 1) {
            mt_s_NAME_dst_mem[mt_s_NAME_args.dst_offset[0] + x * mt_s_NAME_args.y_elems + y] =
                mt_s_NAME_src_mem[mt_s_NAME_args.src_offset[0] + y * mt_s_NAME_args.y_elems + x];
        }
    }
}

@group(0) @binding(0) var<uniform> mt_l_NAME_args: MapTransposeParametersLarge;
@group(0) @binding(1) var<storage, read_write> mt_l_NAME_dst_mem: array<ELEM_TYPE>;
@group(0) @binding(2) var<storage, read_write> mt_l_NAME_src_mem: array<ELEM_TYPE>;
@compute @workgroup_size(map_transpose_large_block_size_x, map_transpose_large_block_size_y, map_transpose_large_block_size_z)
fn map_transpose_NAME_large(
    @builtin(workgroup_id)         group_id: vec3<u32>,  // tblock_id -> unique id of a group  within a dispatch
    @builtin(global_invocation_id) global_id: vec3<u32>, // global_id -> unique id of a thread within a dispatch
    @builtin(local_invocation_id)  local_id: vec3<u32>,  // local_id  -> unique id of a thread within a group
    @builtin(num_workgroups)       num_groups: vec3<u32>
) {
    for (var y = 0; y < mt_l_NAME_args.y_elems[0]; y += 1) {
        for (var x = 0; x < mt_l_NAME_args.x_elems[0]; x += 1) {
            mt_l_NAME_dst_mem[mt_l_NAME_args.dst_offset[0] + x * mt_l_NAME_args.y_elems[0] + y] =
                mt_l_NAME_src_mem[mt_l_NAME_args.src_offset[0] + y * mt_l_NAME_args.y_elems[0] + x];
        }
    }
}

// End of builtin kernel group

// End of builtin_kernels.wgsl
