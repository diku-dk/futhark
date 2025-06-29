override block_size_x: i32 = 1;
override block_size_y: i32 = 1;
override block_size_z: i32 = 1;

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

@group(0) @binding(0) var<uniform> args: CopyParameters;
@group(0) @binding(1) var<storage, read_write> dst_mem: array<ELEM_TYPE>;
@group(0) @binding(2) var<storage, read_write> src_mem: array<ELEM_TYPE>;
@compute @workgroup_size(block_size_x, block_size_y, block_size_z)
fn lmad_copy_NAME(@builtin(global_invocation_id) global_id: vec3<u32>) {
    var remainder: i32 = i32(global_id.x);
    var dst_offset: i32 = args.dst_offset.x;
    var src_offset: i32 = args.src_offset.x;

    if (i32(global_id.x) >= args.n.x) {
      return;
    }

    if (args.r > 0) {
      let i: i32 = remainder % args.shape0.x;
      dst_offset += i * args.dst_stride0.x;
      src_offset += i * args.src_stride0.x;
      remainder  /= args.shape0.x;
    }
    if (args.r > 1) {
      let i: i32 = remainder % args.shape1.x;
      dst_offset += i * args.dst_stride1.x;
      src_offset += i * args.src_stride1.x;
      remainder  /= args.shape1.x;
    }
    if (args.r > 2) {
      let i: i32 = remainder % args.shape2.x;
      dst_offset += i * args.dst_stride2.x;
      src_offset += i * args.src_stride2.x;
      remainder  /= args.shape2.x;
    }
    if (args.r > 3) {
      let i: i32 = remainder % args.shape3.x;
      dst_offset += i * args.dst_stride3.x;
      src_offset += i * args.src_stride3.x;
      remainder  /= args.shape3.x;
    }
    if (args.r > 4) {
      let i: i32 = remainder % args.shape4.x;
      dst_offset += i * args.dst_stride4.x;
      src_offset += i * args.src_stride4.x;
      remainder  /= args.shape4.x;
    }
    if (args.r > 5) {
      let i: i32 = remainder % args.shape5.x;
      dst_offset += i * args.dst_stride5.x;
      src_offset += i * args.src_stride5.x;
      remainder  /= args.shape5.x;
    }
    if (args.r > 6) {
      let i: i32 = remainder % args.shape6.x;
      dst_offset += i * args.dst_stride6.x;
      src_offset += i * args.src_stride6.x;
      remainder  /= args.shape6.x;
    }
    if (args.r > 7) {
      let i: i32 = remainder % args.shape7.x;
      dst_offset += i * args.dst_stride7.x;
      src_offset += i * args.src_stride7.x;
      remainder  /= args.shape7.x;
    }

    write_ELEM_TYPE(&dst_mem, dst_offset, read_ELEM_TYPE(&src_mem, src_offset));
}
