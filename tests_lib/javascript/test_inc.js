// Tests for inc.fut: initialisation, entry points, the utility methods, errors,
// and scalar values.

import assert from "assert/strict";
import { backend, loadLibrary, newFutharkContext, section, ok } from "./harness.js";

const lib = await loadLibrary("inc");

section("FutharkContext");

const fut = await newFutharkContext(lib);

assert.ok(fut instanceof lib.FutharkContext);
assert.equal(typeof fut.entry.inc, "function");
assert.equal(await fut.entry.inc(9), 10);

ok("init() and entry points work");

await assert.rejects(() => fut.entry.inc(), TypeError);
await assert.rejects(() => fut.entry.inc(1, 2), TypeError);

ok("entry points check the number of arguments");

const unused = new lib.FutharkContext();

await assert.rejects(() => unused.context_sync(), /not been initialised/);

ok("a FutharkContext cannot be used before init()");

section("Utility methods");

await fut.context_sync();
await fut.clear_caches();
assert.equal(typeof (await fut.report()), "string");
await fut.pause_profiling();
await fut.unpause_profiling();

ok("context_sync(), clear_caches(), report(), pause_profiling(), and unpause_profiling() work");

section("init() options");

await assert.rejects(() => fut.init(), /only be called once/);
await assert.rejects(() => new lib.FutharkContext().init({ no_such_option: 1 }), /no_such_option/);

ok("init() may only be called once, and rejects unknown options");

await assert.rejects(() => new lib.FutharkContext().init({ debugging: 1 }), TypeError);
await assert.rejects(() => new lib.FutharkContext().init({ tuning_params: { no_such_param: 1 } }),
                     /no_such_param/);
await assert.rejects(() => new lib.FutharkContext().init({ tuning_params: { tunable_x: -1 } }),
                     RangeError);

assert.equal(await fut.entry.tunable(2n), 4n);

const tuned = await newFutharkContext(lib, { tuning_params: { tunable_x: 3 }, profiling: true });

assert.equal(await tuned.entry.tunable(2n), 3n);
assert.equal(typeof (await tuned.report()), "string");
tuned.free();

ok("init() sets flags and tuning parameters, and checks them");

if (backend == "wasm-multicore") {
  const fut2 = await newFutharkContext(lib, { num_threads: 2 });
  assert.equal(await fut2.entry.inc(1), 2);
  fut2.free();
  await assert.rejects(() => new lib.FutharkContext().init({ num_threads: 1.5 }), RangeError);
  ok("init() accepts num_threads");
} else {
  await assert.rejects(() => new lib.FutharkContext().init({ num_threads: 2 }), /num_threads/);
  ok("init() rejects num_threads");
}

section("Errors");

await assert.rejects(() => fut.entry.fail(0), (e) => e instanceof Error && /Assertion/.test(e.message));
assert.equal(await fut.entry.fail(1), 1);

ok("a failing entry point rejects with an Error, and the context remains usable");

section("Scalars");

const ints = [
  ["i8", 8, true],
  ["i16", 16, true],
  ["i32", 32, true],
  ["i64", 64, true],
  ["u8", 8, false],
  ["u16", 16, false],
  ["u32", 32, false],
  ["u64", 64, false],
];

for (const [t, bits, signed] of ints) {
  const f = fut.entry["id_" + t];
  const big = bits == 64;
  const lo = signed ? -(2n ** BigInt(bits - 1)) : 0n;
  const hi = (signed ? 2n ** BigInt(bits - 1) : 2n ** BigInt(bits)) - 1n;
  const conv = big ? (x) => x : Number;
  assert.equal(await f(conv(lo)), conv(lo), t);
  assert.equal(await f(conv(hi)), conv(hi), t);
  assert.equal(await f(1), conv(1n), t);
  assert.equal(await f(1n), conv(1n), t);
  await assert.rejects(() => f(big ? lo - 1n : Number(lo) - 1), RangeError, t);
  await assert.rejects(() => f(big ? hi + 1n : Number(hi) + 1), RangeError, t);
  await assert.rejects(() => f(1.5), RangeError, t);
  await assert.rejects(() => f("1"), TypeError, t);
}

ok("integers of every type are passed and returned, and checked to be in range");

assert.equal(await fut.entry.id_f32(0.1), Math.fround(0.1));
assert.equal(await fut.entry.id_f64(0.1), 0.1);
assert.ok(Number.isNaN(await fut.entry.id_f64(NaN)));
assert.ok(Object.is(await fut.entry.id_f32(-0), -0));
await assert.rejects(() => fut.entry.id_f64("1"), TypeError);
assert.equal(await fut.entry.id_bool(true), true);
assert.equal(await fut.entry.id_bool(false), false);
await assert.rejects(() => fut.entry.id_bool(1), TypeError);

ok("floats and booleans are passed and returned");

// f16 values are rounded to nearest even.
const f16_cases = [
  [1.5, 1.5],
  [0.1, 0.0999755859375],
  [65504, 65504],
  [65519, 65504],
  [65520, Infinity],
  [-1e10, -Infinity],
  [2 ** -24, 2 ** -24],
  [2 ** -25, 0],
  [3 * 2 ** -25, 2 * 2 ** -24],
  [2 ** -14, 2 ** -14],
  [1 + 2 ** -11, 1],
  [1 + 3 * 2 ** -11, 1 + 2 * 2 ** -10],
];

for (const [x, expected] of f16_cases) {
  assert.equal(await fut.entry.id_f16(x), expected, `f16 ${x}`);
  assert.equal(await fut.entry.f16_of_f64(x), expected, `f16.f64 ${x}`);
}
assert.ok(Object.is(await fut.entry.id_f16(-0), -0));
assert.ok(Number.isNaN(await fut.entry.id_f16(NaN)));

if (Math.f16round) {
  for (let i = 0; i < 1000; i++) {
    const x = (Math.random() - 0.5) * 2 ** (Math.random() * 40 - 25);
    assert.equal(await fut.entry.id_f16(x), Math.f16round(x), `f16 ${x}`);
  }
}

ok("f16 values are passed and returned as numbers");

section("Freeing");

fut.free();

await assert.rejects(() => fut.entry.inc(1), /freed/);

ok("a freed FutharkContext cannot be used");

console.log();
console.log("inc tests complete");
