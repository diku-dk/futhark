// Constants used for transpositions. In principle these should be configurable.
const TR_BLOCK_DIM:        i32 = 16;
const TR_TILE_DIM:         i32 = 32;
const TR_ELEMS_PER_THREAD: i32 = 8;

override map_transpose_large_block_size_x: i32 = 1;
override map_transpose_large_block_size_y: i32 = 1;
override map_transpose_large_block_size_z: i32 = 1;

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

var<workgroup> transpose_shared_memory_ELEM_TYPE: array<ELEM_TYPE, TR_TILE_DIM*(TR_TILE_DIM+1)>;

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
            let dst_idx = mt_l_NAME_args.dst_offset[0] + x * mt_l_NAME_args.y_elems[0] + y;
            let src_idx = mt_l_NAME_args.src_offset[0] + y * mt_l_NAME_args.y_elems[0] + x;
            write_ELEM_TYPE(&mt_l_NAME_dst_mem, dst_idx, read_ELEM_TYPE(&mt_l_NAME_src_mem, src_idx));
        }
    }
}