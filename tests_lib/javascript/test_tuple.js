// Tests for tuple.fut, exercising record projection via the FutharkContext API.

import assert from 'assert/strict';

// Hack for Running generated ES6 modules from Emscripten with Node
// https://github.com/emscripten-core/emscripten/issues/11792#issuecomment-877120580
import {dirname} from "path";
import { createRequire } from 'module';
// substring removes file:// from the filepath
globalThis.__dirname = dirname(import.meta.url).substring(7);
globalThis.require = createRequire(import.meta.url);

// Imports from the generated ES6 Module
import { newFutharkContext } from './tuple.mjs';

newFutharkContext().then(fc => {

  console.log("Testing tuple projection...");

  var pair = fc.mk_pair(3, 5n);

  // Project the first field (i32) and second field (i64) of the pair.
  var fst = fc.project_opaque_pair_0(pair);
  var snd = fc.project_opaque_pair_1(pair);

  assert(fst === 3);
  assert(snd === 5n);

  pair.free();

  fc.free();

  console.log("Tuple projection tests complete.");
});
