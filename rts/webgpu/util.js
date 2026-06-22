// Start of util.js

function futhark_assert(condition, message) {
  if (!condition) {
    throw new Error(message || "Assertion failed");
  }
}

function make_prim_info(tag, size, scalar_type, array_type, create_array, get_heap) {
  return {
    tag: tag, // tag used in the binary data format
    size: size,
    scalar_type: scalar_type,
    array_type: array_type,
    get_heap: get_heap,
    create_array: create_array,
  };
}

const primInfos = {
  'bool': make_prim_info("bool", 1, Boolean, Uint8Array,     (h, ...args) => new Uint8Array(h, ...args),     (m) => m.HEAPU8),
    'u8': make_prim_info("  u8", 1, Number,  Uint8Array,     (h, ...args) => new Uint8Array(h, ...args),     (m) => m.HEAPU8),
    'i8': make_prim_info("  i8", 1, Number,  Int8Array,      (h, ...args) => new Int8Array(h, ...args),      (m) => m.HEAP8),
   'u16': make_prim_info(" u16", 2, Number,  Uint16Array,    (h, ...args) => new Uint16Array(h, ...args),    (m) => m.HEAPU16),
   'i16': make_prim_info(" i16", 2, Number,  Int16Array,     (h, ...args) => new Int16Array(h, ...args),     (m) => m.HEAP16),
   'u32': make_prim_info(" u32", 4, Number,  Uint32Array,    (h, ...args) => new Uint32Array(h, ...args),    (m) => m.HEAPU32),
   'i32': make_prim_info(" i32", 4, Number,  Int32Array,     (h, ...args) => new Int32Array(h, ...args),     (m) => m.HEAP32),
   'u64': make_prim_info(" u64", 8, BigInt,  BigUint64Array, (h, ...args) => new BigUint64Array(h, ...args), (m) => m.HEAPU64),
   'i64': make_prim_info(" i64", 8, BigInt,  BigInt64Array,  (h, ...args) => new BigInt64Array(h, ...args),  (m) => m.HEAP64),
   // There is no WASM heap for f16 values since Float16Array was only recently (april 2025) made available in browser baselines,
   // so we have to do this ugly workaround to reinterpret Uint16 bytes as Float16 when reading from the WASM HEAPU16...
   'f16': make_prim_info(" f16", 2, Number,  Float16Array,   (h, ...args) => new Float16Array(new Uint16Array(h, ...args).buffer), (m) => m.HEAPU16),
   'f32': make_prim_info(" f32", 4, Number,  Float32Array,   (h, ...args) => new Float32Array(h, ...args),   (m) => m.HEAPF32),
   'f64': make_prim_info( "f64", 8, Number,  Float64Array,   (h, ...args) => new Float64Array(h, ...args),   (m) => m.HEAPF64),
};

// End of util.js
