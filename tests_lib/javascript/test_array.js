// Tests for array.fut

import assert from 'assert/strict';

// Hack for Running generated ES6 modules from Emscripten with Node
// https://github.com/emscripten-core/emscripten/issues/11792#issuecomment-877120580
import {dirname} from "path";
import { createRequire } from 'module';
// substring removes file:// from the filepath
globalThis.__dirname = dirname(import.meta.url).substring(7);
globalThis.require = createRequire(import.meta.url);

// Imports from the generated ES6 Module
import { newFutharkContext, FutharkArray } from './array.mjs';

newFutharkContext().then(fc => {

  console.log();
  console.log("Testing Entry Points...");
  console.log();

  console.log("Testing Entry Point : sum1d");

  var arr_1d = new Int32Array([1, 2, 3, 4, 5, 6]);
  var fut_arr_1d = fc.new_i32_1d(arr_1d, arr_1d.length);

  var sum1d_res = fc.sum1d(fut_arr_1d);

  assert(sum1d_res === 21);

  console.log("Testing Entry Point : sum2d");
    
  var arr_2d = new BigInt64Array([1n, 2n, 3n, 4n, 5n, 6n]);
  var fut_arr_2d = fc.new_i64_2d(arr_2d, 2, 3);

  var sum2d_res = fc.sum2d(fut_arr_2d);
  assert(sum2d_res === 21n);

  console.log("Testing Entry Point : replicate1d");

  var n = 5n;
  var fut_res_arr_1d = fc.replicate1d(n, 1.1);

  var replicate1d_res_arr = fut_res_arr_1d.toArray();

  for (var i = 0; i < Number(n); i++) {
    // check with consideration for floating point precision
    assert(Math.abs(replicate1d_res_arr[i] - 1.1) < .0001);
  }

  console.log("Testing Entry Point : replicate2d");

  var x = 5n;
  var y = 2n;
  var fut_res_arr_2d = fc.replicate2d(x, y, 1.1);

  var replicate1d_res_arr = fut_res_arr_2d.toArray();

  for (var i = 0; i < Number(x); i++) {
    for (var j = 0; j < Number(y); j++) {
      // check with consideration for floating point precision
      assert(Math.abs(replicate1d_res_arr[i][j] - 1.1) < .0001);
    }
  }

  console.log();
  console.log("Array API Tests...");
  console.log();

  console.log("Testing array construction with numbers and bigints");

  var test_arr = [1n, 2n, 3n, 4n, 5n, 6n];

  var futhark_test_array_shape_ints= fc.new_i64_2d(test_arr, 2, 3);
  var futhark_test_array_shape_bigints = fc.new_i64_2d(test_arr, 2n, 3n);

  var shape_ints = futhark_test_array_shape_ints.shape();
  var shape_bigints = futhark_test_array_shape_bigints.shape();

  assert(shape_ints[0] === shape_bigints[0]);
  assert(shape_ints[1] === shape_bigints[1]);

  console.log("Testing toArray");

  var futhark_test_array = fc.new_i64_2d(test_arr, 2, 3);
  var arr_toArray = futhark_test_array.toArray();

  for (var i = 0; i < 2; i++) {
    for (var j = 0; j < 3; j++) {
      assert(arr_toArray[i][j] === test_arr[i * 3 + j]);
    }
  }

  console.log("Testing toTypedArray");

  var arr_toTypedArray = futhark_test_array.toTypedArray();

  for (var i = 0; i < 3 * 2; i++) {
    assert(arr_toTypedArray[i] === test_arr[i]);
  }

  console.log("Testing shape");

  var expected_shape = [2n, 3n];
  var actual_shape = futhark_test_array.shape();

  assert(actual_shape[0] === actual_shape[0]);
  assert(expected_shape[1] === expected_shape[1]);

  console.log("Testing frees");

  fut_arr_1d.free();
  fut_arr_2d.free();
  fut_res_arr_2d.free();
  fut_res_arr_1d.free();
  futhark_test_array.free();

  console.log("Testing access after free")

  assert.throws(() => fut_test_array.toArray());
  assert.throws(() => fut_test_array.toTypedArray());
  assert.throws(() => fut_test_array.shape());

  fc.free();

  console.log();
  console.log("Tests complete");
  console.log();
});
