// Start of util.js

function futhark_assert(condition, message) {
  if (!condition) {
    throw new Error(message || "Assertion failed");
  }
}

function make_prim_info(tag, size, scalar_type, array_type, get_heap) {
  return {
    tag: tag, // tag used in the binary data format
    size: size,
    scalar_type: scalar_type,
    array_type: array_type,
    get_heap: get_heap,
  };
}

const primInfos = {
  'bool': make_prim_info("bool", 1, Boolean, Uint8Array, (m) => m.HEAPU8),
    'u8': make_prim_info("  u8", 1, Number, Uint8Array, (m) => m.HEAPU8),
    'i8': make_prim_info("  i8", 1, Number,Int8Array, (m) => m.HEAP8),
   'u16': make_prim_info(" u16", 2, Number, Uint16Array, (m) => m.HEAPU16),
   'i16': make_prim_info(" i16", 2, Number, Int16Array, (m) => m.HEAP16),
   'u32': make_prim_info(" u32", 4, Number, Uint32Array, (m) => m.HEAPU32),
   'i32': make_prim_info(" i32", 4, Number, Int32Array, (m) => m.HEAP32),
   'u64': make_prim_info(" u64", 8, BigInt, BigUint64Array, (m) => m.HEAPU64),
   'i64': make_prim_info(" i64", 8, BigInt, BigInt64Array, (m) => m.HEAP64),
   'f32': make_prim_info(" f32", 4, Number, Float32Array, (m) => m.HEAPF32),
   'f64': make_prim_info( "f64", 8, Number, Float64Array, (m) => m.HEAPF64),
};

// End of util.js
