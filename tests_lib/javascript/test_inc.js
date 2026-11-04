// Tests for inc.fut: initialisation, entry points, and the utility methods.

import assert from "assert/strict";
import { loadLibrary, newFutharkContext, section, ok } from "./harness.js";

const lib = await loadLibrary("inc");

section("FutharkContext");

const fut = await newFutharkContext(lib);

assert.ok(fut instanceof lib.FutharkContext);
assert.equal(typeof fut.entry.inc, "function");
assert.equal(await fut.entry.inc(9), 10);

ok("init(module) and entry points work");

section("Utility methods");

await fut.context_sync();
await fut.clear_caches();
assert.equal(typeof (await fut.report()), "string");
await fut.pause_profiling();
await fut.unpause_profiling();

ok("context_sync(), clear_caches(), report(), pause_profiling(), and unpause_profiling() work");

section("init() without a module");

await assert.rejects(() => new lib.FutharkContext().init());

ok("init() requires the module");

fut.free();

console.log();
console.log("inc tests complete");
