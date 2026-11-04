// Tests for record.fut: opaque values, and projection of record and tuple
// fields.

import assert from "assert/strict";
import { loadLibrary, newFutharkContext, section, ok } from "./harness.js";

const lib = await loadLibrary("record");
const fut = await newFutharkContext(lib);

section("Tuples");

const pair = await fut.entry.mk_pair(3, 5n);

assert.ok(pair instanceof lib.FutharkOpaque);
assert.equal(await fut.project_opaque_pair_0(pair), 3);
assert.equal(await fut.project_opaque_pair_1(pair), 5n);

pair.free();
assert.throws(() => pair.free());

ok("tuple fields can be projected");

section("Records with array fields");

// The constructor object exists even though []f32 is used only in a record.
assert.equal(fut.types["[]f32"], fut.f32_1d);

const rec = await fut.entry.mk_rec(4n);
const xs = await fut.project_opaque_rec_xs(rec);

assert.ok(xs instanceof lib.FutharkArray);
assert.equal(await fut.project_opaque_rec_n(rec), 4);
assert.equal(await fut.entry.sum_rec(rec), 10);

// The projected array is independent of the record.
rec.free();
assert.deepEqual(await xs.values(), new Float32Array([0, 1, 2, 3]));
xs.free();

ok("array fields are projected as new arrays");

section("Entry points returning tuples");

const res = await fut.entry.tup(3n);

assert.ok(res instanceof lib.FutharkOpaque);
assert.equal(await fut.project_opaque_tup2_i32_arr1d_i64_0(res), 3);

const is = await fut.project_opaque_tup2_i32_arr1d_i64_1(res);

assert.deepEqual(await is.values(), new BigInt64Array([0n, 1n, 2n]));

is.free();
res.free();

ok("a tuple result is a single opaque value");

fut.free();

console.log();
console.log("record tests complete");
