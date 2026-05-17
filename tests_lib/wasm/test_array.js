// Tests for array.fut.
//
// This checks:
//   1. the backwards-compatible WASM API: newFutharkContext() + direct methods
//   2. the shared WebGPU-style API on FutharkContext
//   3. the preferred FutharkModule API

import assert from "assert/strict";

import { dirname } from "path";
import { createRequire } from "module";

globalThis.__dirname = dirname(import.meta.url).substring(7);
globalThis.require = createRequire(import.meta.url);

import Module, {
  newFutharkContext,
  FutharkContext,
  FutharkModule,
} from "./build/array/array.mjs";

function section(title) {
  console.log();
  console.log(`== ${title} ==`);
}

function ok(message) {
  console.log(`  ✓ ${message}`);
}

async function main() {
  console.log();
  console.log("Testing array.fut");

  section("Backwards-compatible WASM API");

  const fc = await newFutharkContext();

  assert.ok(fc instanceof FutharkContext, "newFutharkContext should return a FutharkContext");
  assert.ok(fc instanceof FutharkModule, "FutharkContext should extend FutharkModule");

  const xs_i32 = new Int32Array([1, 2, 3, 4, 5]);
  const fut_xs_i32_old = fc.new_i32_1d(xs_i32, xs_i32.length);
  const old_sum_i32 = fc.sum_i32_1d(fut_xs_i32_old);

  assert.equal(old_sum_i32, 15);
  ok("newFutharkContext, FutharkContext, and direct entry calls still work");

  const xs_i64 = new BigInt64Array([1n, 2n, 3n, 4n, 5n, 6n]);
  const fut_xs_i64_old = fc.new_i64_2d(xs_i64, 2, 3);
  const old_sum_i64 = fc.sum_i64_2d(fut_xs_i64_old);

  assert.equal(old_sum_i64, 21n);
  ok("old array constructors still work");

  section("Shared WebGPU-style API on FutharkContext");

  assert.ok(fc.entry, "fc.entry should exist");
  assert.ok(fc.i32_1d, "fc.i32_1d should exist");
  assert.ok(fc.i64_2d, "fc.i64_2d should exist");
  assert.equal(typeof fc.i32_1d.from_data, "function");
  assert.equal(typeof fc.i32_1d.from_jsarray, "function");
  assert.equal(typeof fc.i64_2d.from_data, "function");

  const fut_xs_i32_new = fc.i32_1d.from_data(xs_i32, xs_i32.length);
  const new_sum_i32 = await fc.entry.sum_i32_1d(fut_xs_i32_new);

  assert.equal(new_sum_i32, 15);

  const fut_xs_i32_jsarray = fc.i32_1d.from_jsarray([1, 2, 3, 4, 5]);
  const jsarray_sum_i32 = await fc.entry.sum_i32_1d(fut_xs_i32_jsarray);

  assert.equal(jsarray_sum_i32, 15);

  const fut_xs_i64_new = fc.i64_2d.from_data(xs_i64, 2, 3);
  const new_sum_i64 = await fc.entry.sum_i64_2d(fut_xs_i64_new);

  assert.equal(new_sum_i64, 21n);

  ok("entry aliases and array constructor aliases work");

  assert.ok(fc.types["[]i32"], 'fc.types["[]i32"] should exist');
  assert.equal(fc.types["[]i32"], fc.i32_1d);

  assert.ok(fc.types["[][]i64"], 'fc.types["[][]i64"] should exist');
  assert.equal(fc.types["[][]i64"], fc.i64_2d);

  const I32_1D = fc.types["[]i32"];
  const fut_xs_i32_from_type = I32_1D.from_data(xs_i32, xs_i32.length);
  const type_lookup_sum_i32 = await fc.entry.sum_i32_1d(fut_xs_i32_from_type);

  assert.equal(type_lookup_sum_i32, 15);
  ok("type lookup aliases work");

  section("Returned arrays");

  const fut_replicated_1d = await fc.entry.replicate_f32_1d(4n, 1.5);
  const replicated_1d = fut_replicated_1d.toArray();

  assert.equal(replicated_1d.length, 4);
  for (let i = 0; i < replicated_1d.length; i++) {
    assert.ok(Math.abs(replicated_1d[i] - 1.5) < 0.0001);
  }

  const fut_replicated_2d = await fc.entry.replicate_f32_2d(2n, 3n, 2.5);
  const replicated_2d = fut_replicated_2d.toArray();

  assert.equal(replicated_2d.length, 2);
  assert.equal(replicated_2d[0].length, 3);

  for (let i = 0; i < replicated_2d.length; i++) {
    for (let j = 0; j < replicated_2d[i].length; j++) {
      assert.ok(Math.abs(replicated_2d[i][j] - 2.5) < 0.0001);
    }
  }

  ok("old returned-array helpers toArray() and toTypedArray() still work");

  const fut_values_arr = await fc.entry.replicate_f32_1d(3n, 4.5);

  assert.equal(typeof fut_values_arr.values, "function");
  assert.equal(typeof fut_values_arr.get_shape, "function");

  const values = await fut_values_arr.values();
  const shape = fut_values_arr.get_shape();

  assert.ok(values instanceof Float32Array);
  assert.ok(shape instanceof BigInt64Array);

  assert.equal(shape.length, 1);
  assert.equal(shape[0], 3n);

  assert.equal(values.length, 3);
  for (let i = 0; i < values.length; i++) {
    assert.ok(Math.abs(values[i] - 4.5) < 0.0001);
  }

  ok("shared returned-array helpers values() and get_shape() work");

  section("Shared API only");

  const shared_xs = fc.i32_1d.from_data(new Int32Array([10, 20, 30]), 3);
  const shared_sum = await fc.entry.sum_i32_1d(shared_xs);

  assert.equal(shared_sum, 60);

  const shared_arr = await fc.entry.replicate_f32_1d(2n, 7.5);
  const shared_values = await shared_arr.values();
  const shared_shape = shared_arr.get_shape();

  assert.ok(shared_values instanceof Float32Array);
  assert.ok(shared_shape instanceof BigInt64Array);

  assert.equal(shared_shape[0], 2n);
  assert.equal(shared_values.length, 2);
  assert.ok(Math.abs(shared_values[0] - 7.5) < 0.0001);
  assert.ok(Math.abs(shared_values[1] - 7.5) < 0.0001);

  ok("FutharkContext can be used through the shared API only");

  section("Preferred FutharkModule API");

  const module = await Module();
  const fut = new FutharkModule();
  await fut.init(module);

  assert.ok(fut instanceof FutharkModule, "fut should be a FutharkModule");
  assert.ok(fut.entry, "fut.entry should exist");
  assert.ok(fut.i32_1d, "fut.i32_1d should exist");
  assert.ok(fut.i64_2d, "fut.i64_2d should exist");
  assert.ok(fut.types["[]i32"], 'fut.types["[]i32"] should exist');
  assert.ok(fut.types["[][]i64"], 'fut.types["[][]i64"] should exist');

  const module_xs = fut.i32_1d.from_data(new Int32Array([1, 2, 3]), 3);
  const module_sum = await fut.entry.sum_i32_1d(module_xs);

  assert.equal(module_sum, 6);

  const module_arr = await fut.entry.replicate_f32_1d(2n, 8.5);
  const module_values = await module_arr.values();
  const module_shape = module_arr.get_shape();

  assert.ok(module_values instanceof Float32Array);
  assert.ok(module_shape instanceof BigInt64Array);
  assert.equal(module_shape[0], 2n);
  assert.equal(module_values.length, 2);
  assert.ok(Math.abs(module_values[0] - 8.5) < 0.0001);
  assert.ok(Math.abs(module_values[1] - 8.5) < 0.0001);

  ok("FutharkModule supports entry, types, array constructors, values(), and get_shape()");

  section("Cleanup");

  fut_xs_i32_old.free();
  fut_xs_i32_new.free();
  fut_xs_i32_from_type.free();
  fut_xs_i32_jsarray.free();
  fut_xs_i64_old.free();
  fut_xs_i64_new.free();
  fut_replicated_1d.free();
  fut_replicated_2d.free();
  fut_values_arr.free();
  shared_xs.free();
  shared_arr.free();
  module_xs.free();
  module_arr.free();

  fc.free();
  fut.free();

  ok("all allocated arrays and contexts freed");

  console.log();
  console.log("array tests complete");
  console.log();
}

main();