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

section("Invalid construction");

assert.throws(() => fut.i32_1d.from_data([1, 2, 3], 2), RangeError);
assert.throws(() => fut.i32_1d.from_data([1, 2], 1, 2), TypeError);
assert.throws(() => fut.i32_1d.from_data([], -1), RangeError);
assert.throws(() => fut.i32_1d.from_data([], 0.5), RangeError);
assert.throws(() => fut.i32_1d.from_data([1.5], 1), RangeError);
assert.throws(() => fut.i32_1d.from_data([2 ** 31], 1), RangeError);
assert.throws(() => fut.i32_1d.from_data("abc", 3), TypeError);
assert.throws(() => fut.i32_1d.from_data(new DataView(new ArrayBuffer(4)), 1), TypeError);
assert.throws(() => fut.i32_1d.from_data(new Float64Array([1.5]), 1), RangeError);
assert.throws(() => fut.f32_1d.from_data(["1"], 1), TypeError);
assert.throws(() => fut.bool_1d.from_data([2], 1), TypeError);

ok("from_data() checks the data and the shape");

section("Empty arrays");

const empty = fut.i32_1d.from_data([], 0);

assert.equal(await fut.entry.sum_i32_1d(empty), 0);
assert.deepEqual(await empty.values(), new Int32Array(0));

const empty_2d = await fut.entry.replicate_f32_2d(0n, 3n, 1);

assert.deepEqual(empty_2d.shape(), new BigInt64Array([0n, 3n]));
assert.deepEqual(await empty_2d.values(), new Float32Array(0));

empty.free();
empty_2d.free();

ok("empty arrays work");

section("Invalid arguments");

const f32s = fut.f32_1d.from_data([1], 1);

await assert.rejects(() => fut.entry.sum_i32_1d(f32s), TypeError);
await assert.rejects(() => fut.entry.sum_i32_1d([1, 2]), TypeError);
await assert.rejects(() => fut.entry.sum_i32_1d(xs), /freed/);

f32s.free();

const fut2 = await newFutharkContext(lib);
const other = fut2.i32_1d.from_data([1], 1);

await assert.rejects(() => fut.entry.sum_i32_1d(other), /different FutharkContext/);

other.free();
fut2.free();

assert.throws(() => other.shape(), /freed/);

ok("entry points check that arrays have the right type and context, and are not freed");

section("Consumption");

const cs = fut.i32_1d.from_data([1, 2, 3], 3);
const cs_res = await fut.entry.consume_i32_1d(cs);

assert.deepEqual(await cs_res.values(), new Int32Array([42, 2, 3]));
await assert.rejects(() => cs.values(), /consumed/);
assert.throws(() => cs.shape(), /consumed/);
await assert.rejects(() => fut.entry.sum_i32_1d(cs), /consumed/);
cs.free();

const ds = fut.i32_1d.from_data([1, 2, 3], 3);

await assert.rejects(() => fut.entry.consume_first(ds, ds), /consumed/);
assert.equal(await fut.entry.sum_i32_1d(ds), 6);
assert.equal(await fut.entry.consume_first(ds, cs_res), 5 + 47);
await assert.rejects(() => ds.values(), /consumed/);

ds.free();
cs_res.free();

ok("consumed arrays can only be freed, and cannot be passed twice");

section("Concurrent operations");

const qs = fut.i32_1d.from_data([1, 2, 3], 3);

// Arguments are checked when the call runs, after 'qs' has been consumed.
const r1 = fut.entry.consume_i32_1d(qs);
const r2 = fut.entry.sum_i32_1d(qs);
const qs_res = await r1;

await assert.rejects(r2, /consumed/);
assert.deepEqual(await Promise.all([1, 2, 3, 4].map(() => fut.entry.sum_i32_1d(qs_res))),
                 [47, 47, 47, 47]);

// Settle the queue, so the next operation starts immediately.
await new Promise((resolve) => setTimeout(resolve, 0));
const pending = fut.entry.sum_i32_1d(qs_res);
await Promise.resolve();

assert.throws(() => qs_res.shape(), /running/);
assert.throws(() => qs_res.free(), /running/);
assert.throws(() => fut.i32_1d.from_data([1], 1), /running/);
assert.throws(() => fut.free(), /running/);
assert.equal(await pending, 47);
assert.deepEqual(qs_res.shape(), new BigInt64Array([3n]));

qs.free();
qs_res.free();

ok("asynchronous operations run one at a time, and synchronous operations cannot run meanwhile");

section("f16 and bool arrays");

const hs = fut.f16_1d.from_data([1, 3, 0.1], 3);
const hs_res = await fut.entry.halve_f16_1d(hs);
const hs_values = await hs_res.values();

assert.ok(hs_values instanceof (globalThis.Float16Array ?? Float32Array));
assert.deepEqual(Array.from(hs_values), [0.5, 1.5, 0.0999755859375 / 2]);

const hs32 = fut.f16_1d.from_data(new Float32Array([4]), 1n);

assert.deepEqual(Array.from(await hs32.values()), [4]);

for (const arr of [hs, hs_res, hs32]) {
  arr.free();
}

const bs = fut.bool_1d.from_data([true, false, true], 3);
const bs_res = await fut.entry.not_bool_1d(bs);

assert.deepEqual(await bs_res.values(), new Uint8Array([0, 1, 0]));

const bs_again = fut.bool_1d.from_data(await bs_res.values(), 3);

assert.deepEqual(await bs_again.values(), new Uint8Array([0, 1, 0]));
bs_again.free();

bs.free();
bs_res.free();

ok("f16 arrays hold numbers, and bool arrays are returned as Uint8Arrays");

section("Memory growth");

// Larger than the initial WASM memory, so it must grow.
const big = await fut.entry.replicate_f32_1d(50000000n, 3.5);

assert.deepEqual(await zsss.values(), Int32Array.from({ length: 24 }, (_, i) => i));
assertClose((await big.values())[49999999], 3.5);

ok("arrays created before the memory grows remain usable");

big.free();
zsss.free();
fut.free();

console.log();
console.log("array tests complete");
