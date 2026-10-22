// Constants used for transpositions. In principle these should be configurable.
const TR_BLOCK_DIM:        i32 = 16;
const TR_TILE_DIM:         i32 = 32;
const TR_ELEMS_PER_THREAD: i32 = 8;

override block_size_x: i32 = 1;
override block_size_y: i32 = 1;
override block_size_z: i32 = 1;

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

@group(0) @binding(0) var<uniform> args: MapTransposeParameters;
@group(0) @binding(1) var<storage, read_write> dst_mem: array<ELEM_TYPE>;
@group(0) @binding(2) var<storage, read_write> src_mem: array<ELEM_TYPE>;
@compute @workgroup_size(block_size_x, block_size_y, block_size_z)
fn map_transpose_NAME_small(
    @builtin(workgroup_id)         group_id: vec3<u32>,  // tblock_id -> unique id of a group  within a dispatch
    @builtin(global_invocation_id) global_id: vec3<u32>, // global_id -> unique id of a thread within a dispatch
    @builtin(local_invocation_id)  local_id: vec3<u32>,  // local_id  -> unique id of a thread within a group
    @builtin(num_workgroups)       num_groups: vec3<u32>
) {
    let dst_offset = args.dst_offset[0];
    let src_offset = args.src_offset[0];

    let global_id_0 = i32(global_id[0]);

    let our_array_offset = global_id_0 / (args.y_elems * args.x_elems) * args.y_elems * args.x_elems;
    let x_index = (global_id_0 % (args.y_elems * args.x_elems)) / args.y_elems;
    let y_index = global_id_0 % args.y_elems;

    let odata_offset = dst_offset + our_array_offset;
    let idata_offset = src_offset + our_array_offset;
    let index_in = y_index * args.x_elems + x_index;
    let index_out = x_index * args.y_elems + y_index;

    if (global_id_0 < args.x_elems * args.y_elems * args.num_arrays) {
        write_ELEM_TYPE(&dst_mem, odata_offset + index_out, read_ELEM_TYPE(&src_mem, idata_offset + index_in));
    }
}
