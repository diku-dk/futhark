// Tests for inc.fut.
//
// This checks:
//   1. the backwards-compatible WASM API: newFutharkContext() + direct methods
//   2. the shared entry API on FutharkContext
//   3. the preferred FutharkModule API with explicit module initialisation
//   4. that FutharkModule.init() requires a module argument
//   5. the shared utility methods on FutharkModule

import assert from "assert/strict";

// Hack for running generated ES6 modules from Emscripten with Node.
// https://github.com/emscripten-core/emscripten/issues/11792#issuecomment-877120580
import { dirname } from "path";
import { createRequire } from "module";

// substring removes file:// from the filepath
globalThis.__dirname = dirname(import.meta.url).substring(7);
globalThis.require = createRequire(import.meta.url);

// Imports from the generated ES6 module.
import Module, {
  newFutharkContext,
  FutharkContext,
  FutharkModule,
} from "./build/inc/inc.mjs";

function section(title) {
  console.log();
  console.log(`== ${title} ==`);
}

function ok(message) {
  console.log(`  ✓ ${message}`);
}

async function main() {
  console.log();
  console.log("Testing inc.fut");

  section("Backwards-compatible WASM API");

  const fc = await newFutharkContext();

  assert.ok(
    fc instanceof FutharkContext,
    "newFutharkContext should return a FutharkContext"
  );
  assert.ok(
    fc instanceof FutharkModule,
    "FutharkContext should extend FutharkModule"
  );

  const old_api_res = fc.inc(9);
  assert.equal(old_api_res, 10);

  ok("newFutharkContext, FutharkContext, and direct entry calls still work");

  section("Shared entry API on FutharkContext");

  assert.ok(fc.entry, "fc.entry should exist");
  assert.equal(typeof fc.entry.inc, "function");

  const entry_api_res = await fc.entry.inc(9);
  assert.equal(entry_api_res, 10);

  ok("FutharkContext exposes entry aliases");

  section("Preferred FutharkModule API");

  const fut_without_module = new FutharkModule();

  await assert.rejects(
    () => fut_without_module.init(),
    /requires the generated backend runtime module/
  );

  ok("FutharkModule.init() requires an explicit module argument");

  const module = await Module();
  const fut = new FutharkModule();

  assert.ok(fut instanceof FutharkModule, "fut should be a FutharkModule");

  await fut.init(module);

  assert.ok(fut.entry, "fut.entry should exist");
  assert.equal(typeof fut.entry.inc, "function");

  const module_api_res = await fut.entry.inc(9);
  assert.equal(module_api_res, 10);

  ok("FutharkModule supports explicit init(module) and entry aliases");

  section("Shared utility methods");

  assert.equal(typeof fut.context_sync, "function");
  assert.equal(typeof fut.clear_caches, "function");
  assert.equal(typeof fut.report, "function");
  assert.equal(typeof fut.pause_profiling, "function");
  assert.equal(typeof fut.unpause_profiling, "function");

  await fut.context_sync();
  await fut.clear_caches();

  const report = await fut.report();
  assert.equal(typeof report, "string");

  await fut.pause_profiling();
  await fut.unpause_profiling();

  ok("context_sync(), clear_caches(), report(), pause_profiling(), and unpause_profiling() work");

  section("Cleanup");

  fc.free();
  fut.free();

  ok("all contexts freed");

  console.log();
  console.log("inc tests complete");
  console.log();
}

main();