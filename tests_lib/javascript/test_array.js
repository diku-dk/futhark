// Tests for array.fut: array construction, passing arrays to entry points, and
// reading returned arrays.

import assert from "assert/strict";
import { loadLibrary, newFutharkContext, section, ok } from "./harness.js";

function assertClose(actual, expected) {
  assert.ok(Math.abs(actual - expected) < 0.0001, `${actual} is not ${expected}`);
}

const lib = await loadLibrary("array");
const fut = await newFutharkContext(lib);

section("Array types");

assert.equal(fut.types["[]i32"], fut.i32_1d);
assert.equal(fut.types["[][]i64"], fut.i64_2d);
assert.equal(fut.types["[]f32"], fut.f32_1d);
assert.equal(fut.types["[][]f32"], fut.f32_2d);
assert.equal(fut.types["[][][]i32"], fut.i32_3d);

ok("constructor objects are available in types and as e.g. i32_1d");

section("Array construction");

const xs = fut.i32_1d.from_data(new Int32Array([1, 2, 3, 4, 5]), 5);
const xs_plain = fut.i32_1d.from_data([1, 2, 3, 4, 5], 5);
const xss = fut.i64_2d.from_data(new BigInt64Array([1n, 2n, 3n, 4n, 5n, 6n]), 2, 3);
const xss_bigint = fut.i64_2d.from_data([1n, 2n, 3n, 4n, 5n, 6n], 2n, 3n);

assert.ok(xs instanceof lib.FutharkArray);
assert.deepEqual(xs.shape(), new BigInt64Array([5n]));
assert.deepEqual(xss.shape(), new BigInt64Array([2n, 3n]));
assert.deepEqual(xss_bigint.shape(), xss.shape());

assert.equal(await fut.entry.sum_i32_1d(xs), 15);
assert.equal(await fut.entry.sum_i32_1d(xs_plain), 15);
assert.equal(await fut.entry.sum_i64_2d(xss), 21n);
assert.equal(await fut.entry.sum_i64_2d(xss_bigint), 21n);

ok("from_data() accepts typed arrays, Arrays, and number or BigInt dimensions");

section("Returned arrays");

const ys = await fut.entry.replicate_f32_1d(4n, 1.5);
const ys_values = await ys.values();

assert.ok(ys_values instanceof Float32Array);
assert.deepEqual(ys.shape(), new BigInt64Array([4n]));
assert.equal(ys_values.length, 4);
ys_values.forEach((y) => assertClose(y, 1.5));

// The result of values() is a copy.
ys_values[0] = 0;
assertClose((await ys.values())[0], 1.5);

const yss = await fut.entry.replicate_f32_2d(2n, 3n, 2.5);
const yss_values = await yss.values();

assert.deepEqual(yss.shape(), new BigInt64Array([2n, 3n]));
assert.equal(yss_values.length, 6);
yss_values.forEach((y) => assertClose(y, 2.5));

const zsss = await fut.entry.iota_i32_3d(2n, 3n, 4n);

assert.deepEqual(zsss.shape(), new BigInt64Array([2n, 3n, 4n]));
assert.deepEqual(await zsss.values(), Int32Array.from({ length: 24 }, (_, i) => i));

ok("values() and shape() work");

section("Freeing");

for (const arr of [xs, xs_plain, xss, xss_bigint, ys, yss]) {
  arr.free();
}

assert.throws(() => xs.shape());
assert.throws(() => xs.free());
await assert.rejects(async () => await ys.values());

ok("arrays cannot be used after being freed");

yss_values.forEach((y) => assertClose(y, 2.5));

ok("the result of values() remains valid after freeing");

zsss.free();
fut.free();

console.log();
console.log("array tests complete");
