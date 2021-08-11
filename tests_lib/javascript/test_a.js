// Tests for a.fut, the increment function

import assert from 'assert/strict';

// Hack for Running generated ES6 modules from Emscripten with Node
// https://github.com/emscripten-core/emscripten/issues/11792#issuecomment-877120580
import {dirname} from "path";
import { createRequire } from 'module';
// substring removes file:// from the filepath
globalThis.__dirname = dirname(import.meta.url).substring(7);
globalThis.require = createRequire(import.meta.url);

// Imports from the generated ES6 Module
import { newFutharkContext } from './a.mjs';

var fc;
newFutharkContext().then(x => {
  fc = x;
  var res = fc.incr(7)
  assert(res == 8);
});
