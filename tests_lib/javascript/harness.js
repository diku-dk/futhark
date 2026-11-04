// Things shared by the tests: loading a compiled library for the backend under
// test, and reporting progress.
//
// The backend is given by the FUTHARK_BACKEND environment variable, and the
// compiled libraries are found in BUILD_DIR (both are set by the Makefile).
// Tests should be written against the API shared by all backends (see
// docs/js-api.rst): in particular, a method that is asynchronous on any backend
// should always be awaited.  Tests of backend-specific API must check the
// backend first.

import path from "path";
import { pathToFileURL } from "url";

export const backend = process.env.FUTHARK_BACKEND ?? "wasm";

const build_dir = process.env.BUILD_DIR ?? path.join("build", backend);

// Load the library compiled from the program 'name'.fut.  The result has the
// generated classes (FutharkContext, FutharkArray, FutharkOpaque) as fields.
export async function loadLibrary(name) {
  return await import(pathToFileURL(path.resolve(build_dir, name + ".mjs")));
}

// A new initialised FutharkContext for a library returned by 'loadLibrary'.
export async function newFutharkContext(lib, options) {
  const fut = new lib.FutharkContext();
  await fut.init(options);
  return fut;
}

export function section(title) {
  console.log();
  console.log(`== ${title} ==`);
}

export function ok(message) {
  console.log(`  ✓ ${message}`);
}
