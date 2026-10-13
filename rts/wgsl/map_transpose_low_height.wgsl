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

var<workgroup> shared_memory_ELEM_TYPE: array<ELEM_TYPE, TR_TILE_DIM*(TR_TILE_DIM+1)>;

@group(0) @binding(0) var<uniform> args: MapTransposeParameters;
@group(0) @binding(1) var<storage, read_write> dst_mem: array<ELEM_TYPE>;
@group(0) @binding(2) var<storage, read_write> src_mem: array<ELEM_TYPE>;
@compute @workgroup_size(block_size_x, block_size_y, block_size_z)
fn map_transpose_NAME_low_height(
    @builtin(workgroup_id)         group_id: vec3<u32>,  // tblock_id -> unique id of a group  within a dispatch
    @builtin(global_invocation_id) global_id: vec3<u32>, // global_id -> unique id of a thread within a dispatch
    @builtin(local_invocation_id)  local_id: vec3<u32>,  // local_id  -> unique id of a thread within a group
    @builtin(num_workgroups)       num_groups: vec3<u32>
) {
    let dst_offset = args.dst_offset[0];
    let src_offset = args.src_offset[0];

    let tblock_id_0 = i32(group_id[0]);
    let global_id_0 = i32(global_id[0]);
    var tblock_id_1 = i32(group_id[1]);
    var global_id_1 = i32(global_id[1]);

    for (var i1 = 0; i1 <= args.repeat_1; i1++) {
        var tblock_id_2 = i32(group_id[2]);
        var global_id_2 = i32(global_id[2]);

        for (var i2 = 0; i2 <= args.repeat_2; i2++) {
            let our_array_offset = tblock_id_2 * args.x_elems * args.y_elems;
            let odata_offset = dst_offset + our_array_offset;
            let idata_offset = src_offset + our_array_offset;
            var x_index = tblock_id_0 * TR_BLOCK_DIM * args.mulx +
                          i32(local_id[0]) +
                          (i32(local_id[1]) % args.mulx) * TR_BLOCK_DIM;
            var y_index = tblock_id_1 * TR_BLOCK_DIM + i32(local_id[1]) / args.mulx;
            let index_in = y_index * args.x_elems + x_index;

            if (x_index < args.x_elems && y_index < args.y_elems) {
                let shared_offset = i32(local_id[1]) * (TR_BLOCK_DIM + 1) + i32(local_id[0]);
                let src_val = read_ELEM_TYPE(&src_mem, idata_offset + index_in);
                shared_memory_ELEM_TYPE[shared_offset] = src_val;
            }

            workgroupBarrier();

            x_index = tblock_id_1 * TR_BLOCK_DIM + i32(local_id[0]) / args.mulx;
            y_index = tblock_id_0 * TR_BLOCK_DIM * args.mulx + 
                      i32(local_id[1]) +
                      (i32(local_id[0]) % args.mulx) * TR_BLOCK_DIM;
            let index_out = y_index * args.y_elems + x_index;

            if (x_index < args.y_elems && y_index < args.x_elems) {
                let shared_offset = i32(local_id[0]) * (TR_BLOCK_DIM + 1) + i32(local_id[1]);
                let src_val = ELEM_TYPE(shared_memory_ELEM_TYPE[shared_offset]);
                write_ELEM_TYPE(&dst_mem, odata_offset + index_out, src_val);
            }

            tblock_id_2 += i32(num_groups[2]);
            global_id_2 += i32(num_groups[2]) * block_size_z;
        }

        tblock_id_1 += i32(num_groups[1]);
        global_id_1 += i32(num_groups[1]) * block_size_y;
    }
}