override lmad_copy_block_size_x: i32 = 1;
override lmad_copy_block_size_y: i32 = 1;
override lmad_copy_block_size_z: i32 = 1;

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

    write_ELEM_TYPE(&copy_dst_NAME_mem, dst_offset, read_ELEM_TYPE(&copy_src_NAME_mem, src_offset));
}
