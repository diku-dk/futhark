// Start of wrappers.js
//
// The JavaScript API (docs/js-api.rst), driven by 'futhark_manifest'.
//
// Asynchronous operations run one at a time, as Asyncify (used by WebGPU)
// allows no other calls into the module while one is suspended.

// f16 values and their bit patterns, rounding to nearest even.
function futhark_f16_bits(x) {
  if (Number.isNaN(x)) {
    return 0x7e00;
  }
  var sign = (x < 0 || Object.is(x, -0)) ? 0x8000 : 0;
  x = Math.abs(x);
  if (x < 2 ** -14) {
    // Subnormal or zero.
    return sign | futhark_round_even(x * 2 ** 24);
  }
  var e = Math.floor(Math.log2(x));
  // Math.log2 may be inexact.
  if (2 ** e > x) {
    e--;
  } else if (2 ** (e + 1) <= x) {
    e++;
  }
  var m = futhark_round_even((x / 2 ** e - 1) * 1024);
  if (m == 1024) {
    m = 0;
    e++;
  }
  if (e > 15) {
    return sign | 0x7c00;
  }
  return sign | ((e + 15) << 10) | m;
}

function futhark_round_even(x) {
  var r = Math.floor(x);
  var d = x - r;
  return (d > 0.5 || (d == 0.5 && r % 2 == 1)) ? r + 1 : r;
}

function futhark_f16_value(b) {
  var sign = (b & 0x8000) ? -1 : 1;
  var e = (b >> 10) & 0x1f;
  var m = b & 0x3ff;
  if (e == 0) {
    return sign * m * 2 ** -24;
  } else if (e == 31) {
    return m == 0 ? sign * Infinity : NaN;
  } else {
    return sign * (1 + m / 1024) * 2 ** (e - 15);
  }
}

function futhark_check_int(x, bits, signed, what) {
  var ok;
  if (typeof x == "bigint") {
    ok = (signed ? BigInt.asIntN(bits, x) : BigInt.asUintN(bits, x)) == x;
  } else if (typeof x == "number") {
    var lo = signed ? -(2 ** (bits - 1)) : 0;
    var hi = signed ? 2 ** (bits - 1) : 2 ** bits;
    ok = Number.isInteger(x) && x >= lo && x < hi;
  } else {
    throw new TypeError(what + " must be a number or a BigInt");
  }
  if (!ok) {
    throw new RangeError(what + " is not a " + (signed ? "i" : "u") + bits + " value: " + x);
  }
}

// 'array' is used for values(), and 'c_array' for the C representation.
function futhark_int_prim(bits, signed, array) {
  var big = bits == 64;
  return {
    size: bits / 8,
    array: array,
    c_array: array,
    to_c: (x, what) => {
      futhark_check_int(x, bits, signed, what);
      return big ? BigInt(x) : Number(x);
    },
    get: (dv, p) => big
      ? (signed ? dv.getBigInt64(p, true) : dv.getBigUint64(p, true))
      : dv["get" + (signed ? "Int" : "Uint") + bits](p, true),
    to_values: (bytes) => new array(bytes.buffer),
  };
}

function futhark_float_prim(bits, array) {
  return {
    size: bits / 8,
    array: array,
    c_array: array,
    to_c: (x, what) => {
      if (typeof x != "number") {
        throw new TypeError(what + " must be a number");
      }
      return x;
    },
    get: (dv, p) => dv["getFloat" + bits](p, true),
    to_values: (bytes) => new array(bytes.buffer),
  };
}

// Float32Array represents all f16 values exactly.
var futhark_f16_array = globalThis.Float16Array ?? Float32Array;

var futhark_prims = {
  i8: futhark_int_prim(8, true, Int8Array),
  i16: futhark_int_prim(16, true, Int16Array),
  i32: futhark_int_prim(32, true, Int32Array),
  i64: futhark_int_prim(64, true, BigInt64Array),
  u8: futhark_int_prim(8, false, Uint8Array),
  u16: futhark_int_prim(16, false, Uint16Array),
  u32: futhark_int_prim(32, false, Uint32Array),
  u64: futhark_int_prim(64, false, BigUint64Array),
  f16: {
    size: 2,
    array: futhark_f16_array,
    c_array: Uint16Array,
    to_c: (x, what) => futhark_f16_bits(futhark_prims.f32.to_c(x, what)),
    get: (dv, p) => futhark_f16_value(dv.getUint16(p, true)),
    to_values: (bytes) =>
      futhark_f16_array.from(new Uint16Array(bytes.buffer), futhark_f16_value),
  },
  f32: futhark_float_prim(32, Float32Array),
  f64: futhark_float_prim(64, Float64Array),
  bool: {
    size: 1,
    array: Uint8Array,
    c_array: Uint8Array,
    to_c: (x, what) => {
      if (typeof x != "boolean") {
        throw new TypeError(what + " must be a boolean");
      }
      return x ? 1 : 0;
    },
    get: (dv, p) => dv.getUint8(p) != 0,
    to_values: (bytes) => bytes,
  },
};

// Booleans may also be 0 or 1, as returned by values().
function futhark_from_data(type, prim, data) {
  if (data instanceof prim.c_array && prim.c_array === prim.array && type != "bool") {
    return data;
  }
  var out = new prim.c_array(data.length);
  for (var i = 0; i < data.length; i++) {
    var x = data[i];
    if (type == "bool" && (x === 0 || x === 1)) {
      x = x === 1;
    }
    out[i] = prim.to_c(x, "Element " + i + " of the array data");
  }
  return out;
}

function futhark_check_value(obj, cls, ctx, type, what) {
  if (!(obj instanceof cls)) {
    throw new TypeError(what + " must be a " + cls.name);
  }
  if (obj.ctx !== ctx) {
    throw new Error(what + " belongs to a different FutharkContext");
  }
  if (obj.type !== type) {
    throw new TypeError(what + " has type " + obj.type + ", but must have type " + type);
  }
  obj._check_usable();
}

class FutharkValue {
  constructor(ctx, type, ptr) {
    this.ctx = ctx;
    this.type = type;
    this.ptr = ptr;
    this.consumed = false;
  }

  _check_usable() {
    this._check_not_freed();
    if (this.consumed) {
      throw new Error("Using a value that has been consumed by an entry point");
    }
  }

  _check_not_freed() {
    if (this.ptr === undefined) {
      throw new Error("Using a value that has been freed");
    }
    this.ctx._check_usable();
  }

  free() {
    this._check_not_freed();
    this.ctx._check_idle();
    this.ctx.wasm["_" + this.ctx.manifest.types[this.type].ops.free](this.ctx.ctx, this.ptr);
    this.ptr = undefined;
  }
}

class FutharkArray extends FutharkValue {
  shape() {
    this._check_usable();
    this.ctx._check_idle();
    return this._shape();
  }

  _shape() {
    var info = this.ctx.manifest.types[this.type];
    var p = this.ctx.wasm["_" + info.ops.shape](this.ctx.ctx, this.ptr);
    return new BigInt64Array(this.ctx.wasm.HEAPU8.slice(p, p + 8 * info.rank).buffer);
  }

  values() {
    var ctx = this.ctx;
    return ctx._exclusive(async () => {
      this._check_usable();
      var info = ctx.manifest.types[this.type];
      var prim = futhark_prims[info.elemtype];
      var bytes = Number(this._shape().reduce((a, b) => a * b, 1n)) * prim.size;
      var p = ctx._malloc(bytes);
      try {
        ctx._check(await ctx._call(info.ops.values, ctx.ctx, this.ptr, p));
        await ctx._sync();
        return prim.to_values(ctx.wasm.HEAPU8.slice(p, p + bytes));
      } finally {
        ctx.wasm._free(p);
      }
    });
  }
}

class FutharkOpaque extends FutharkValue {
}

class FutharkContext {
  constructor() {
    this.manifest = futhark_manifest;
    this.wasm = undefined;
    this.cfg = undefined;
    this.ctx = undefined;
    this.initialised = false;
    this.freed = false;
    this.queue = Promise.resolve();
    this.running = false;
    this.entry = Object.create(null);
    this.types = Object.create(null);
  }

  async init(options = {}) {
    if (this.initialised) {
      throw new Error("FutharkContext.init() may only be called once");
    }
    this.initialised = true;
    const flags = ["debugging", "logging", "profiling"];
    for (const option of Object.keys(options)) {
      if (option != "num_threads" && option != "tuning_params" && !flags.includes(option)) {
        throw new Error("Unknown FutharkContext.init() option: " + option);
      }
    }
    for (const flag of flags) {
      if (options[flag] !== undefined && typeof options[flag] != "boolean") {
        throw new TypeError("The " + flag + " option must be a boolean");
      }
    }
    if (options.num_threads !== undefined) {
      futhark_check_int(options.num_threads, 32, true, "The num_threads option");
    }
    const tuning_params = Object.entries(options.tuning_params ?? {});
    for (const [name, value] of tuning_params) {
      futhark_check_int(value, 32, false, "Tuning parameter " + name);
    }

    if (this.manifest.backend == "wasm_multicore" && globalThis.SharedArrayBuffer === undefined) {
      throw new Error("The wasm-multicore backend requires SharedArrayBuffer, which browsers " +
                      "only provide to pages that are cross-origin isolated");
    }
    try {
      this.wasm = await loadWASM();
    } catch (e) {
      throw e instanceof Error ? e : new Error("Failed to load the WebAssembly module: " + e);
    }
    if (options.num_threads !== undefined &&
        this.wasm._futhark_context_config_set_num_threads === undefined) {
      throw new Error("The num_threads option is not supported by this backend");
    }

    this.cfg = this.wasm._futhark_context_config_new();
    for (const flag of flags) {
      if (options[flag]) {
        this.wasm["_futhark_context_config_set_" + flag](this.cfg, 1);
      }
    }
    if (options.num_threads !== undefined) {
      this.wasm._futhark_context_config_set_num_threads(this.cfg, options.num_threads);
    }
    for (const [name, value] of tuning_params) {
      if (this.wasm.ccall("futhark_context_config_set_tuning_param", "number",
                          ["number", "string", "number"], [this.cfg, name, value]) != 0) {
        this.free();
        throw new Error("Unknown tuning parameter: " + name);
      }
    }
    var ctx = await this._call("futhark_context_new", this.cfg);
    var err = this._get_error(ctx);
    if (err !== null) {
      this.wasm._futhark_context_free(ctx);
      this.free();
      throw new Error(err);
    }
    this.ctx = ctx;

    for (const [name, info] of Object.entries(this.manifest.types)) {
      if (info.kind == "array") {
        const constructor = {
          from_data: (data, ...shape) => this._new_array(name, data, shape),
        };
        this.types[name] = constructor;
        this[info.elemtype + "_" + info.rank + "d"] = constructor;
      } else if (info.record !== undefined) {
        for (const field of info.record.fields) {
          this[field.project.replace(/^futhark_/, "")] =
            (obj) => this._exclusive(() => this._project(name, field, obj));
        }
      }
    }
    for (const [name, info] of Object.entries(this.manifest.entry_points)) {
      this.entry[name] = (...args) => this._exclusive(() => this._call_entry(name, info, args));
    }
  }

  free() {
    this._check_idle();
    if (this.ctx !== undefined) {
      this.wasm._futhark_context_free(this.ctx);
      this.ctx = undefined;
    }
    if (this.cfg !== undefined) {
      this.wasm._futhark_context_config_free(this.cfg);
      this.cfg = undefined;
    }
    this.freed = true;
  }

  context_sync() {
    return this._exclusive(async () => {
      this._check_usable();
      await this._sync();
    });
  }

  clear_caches() {
    return this._exclusive(async () => {
      this._check_usable();
      this._check(await this._call("futhark_context_clear_caches", this.ctx));
    });
  }

  report() {
    return this._exclusive(async () => {
      this._check_usable();
      return this._c_string(await this._call("futhark_context_report", this.ctx));
    });
  }

  pause_profiling() {
    return this._exclusive(async () => {
      this._check_usable();
      await this._call("futhark_context_pause_profiling", this.ctx);
    });
  }

  unpause_profiling() {
    return this._exclusive(async () => {
      this._check_usable();
      await this._call("futhark_context_unpause_profiling", this.ctx);
    });
  }

  // Everything below is internal.

  _check_usable() {
    if (this.freed) {
      throw new Error("The FutharkContext has been freed");
    }
    if (this.ctx === undefined) {
      throw new Error("The FutharkContext has not been initialised");
    }
  }

  // Run 'f' after all previously started asynchronous operations.
  _exclusive(f) {
    var run = this.queue.then(async () => {
      this.running = true;
      try {
        return await f();
      } finally {
        this.running = false;
      }
    });
    this.queue = run.catch(() => {});
    return run;
  }

  _check_idle() {
    if (this.running) {
      throw new Error("Cannot do this while an asynchronous operation on the FutharkContext is running");
    }
  }

  async _call(cfun, ...args) {
    return await this.wasm.ccall(cfun, "number", args.map(() => "number"), args, {async: true});
  }

  _malloc(n) {
    // malloc(0) may return NULL.
    var p = this.wasm._malloc(Math.max(n, 1));
    if (p == 0) {
      throw new RangeError("Failed to allocate " + n + " bytes");
    }
    return p;
  }

  _c_string(p) {
    var heap = this.wasm.HEAPU8;
    // Browsers' TextDecoder rejects shared memory.
    var str = new TextDecoder().decode(heap.slice(p, heap.indexOf(0, p)));
    this.wasm._free(p);
    return str;
  }

  _get_error(ctx = this.ctx) {
    var p = this.wasm._futhark_context_get_error(ctx);
    return p == 0 ? null : this._c_string(p);
  }

  _error() {
    return new Error(this._get_error() ?? "Unknown error");
  }

  _check(ret) {
    if (ret != 0) {
      throw this._error();
    }
  }

  async _sync() {
    this._check(await this._call("futhark_context_sync", this.ctx));
  }

  _to_c(type, x, what) {
    if (type in futhark_prims) {
      return futhark_prims[type].to_c(x, what);
    }
    var cls = this.manifest.types[type].kind == "array" ? FutharkArray : FutharkOpaque;
    futhark_check_value(x, cls, this, type, what);
    return x.ptr;
  }

  _from_c(type, p) {
    var dv = new DataView(this.wasm.HEAPU8.buffer);
    if (type in futhark_prims) {
      return futhark_prims[type].get(dv, p);
    }
    var ptr = dv.getUint32(p, true);
    if (this.manifest.types[type].kind == "array") {
      return new FutharkArray(this, type, ptr);
    } else {
      return new FutharkOpaque(this, type, ptr);
    }
  }

  _free_c(type, p) {
    if (!(type in futhark_prims)) {
      var ptr = new DataView(this.wasm.HEAPU8.buffer).getUint32(p, true);
      this.wasm["_" + this.manifest.types[type].ops.free](this.ctx, ptr);
    }
  }

  // For C functions with an output parameter after the context.
  async _call_with_output(cfun, type, args, after_call) {
    var out = this._malloc(8);
    try {
      var ret = await this._call(cfun, this.ctx, out, ...args);
      after_call();
      this._check(ret);
      try {
        await this._sync();
      } catch (e) {
        this._free_c(type, out);
        throw e;
      }
      return this._from_c(type, out);
    } finally {
      this.wasm._free(out);
    }
  }

  _new_array(type, data, shape) {
    this._check_usable();
    this._check_idle();
    var info = this.manifest.types[type];
    var prim = futhark_prims[info.elemtype];
    if (shape.length != info.rank) {
      throw new TypeError("Expected " + info.rank + " dimensions, but got " + shape.length);
    }
    shape = shape.map((d, i) => {
      futhark_check_int(d, 64, true, "dimension " + i);
      if (d < 0) {
        throw new RangeError("dimension " + i + " is negative: " + d);
      }
      return BigInt(d);
    });
    if (!(data instanceof Array || (ArrayBuffer.isView(data) && !(data instanceof DataView)))) {
      throw new TypeError("Array data must be an Array or a typed array");
    }
    var n = shape.reduce((a, b) => a * b, 1n);
    if (BigInt(data.length) != n) {
      throw new RangeError("Array data has " + data.length + " elements, but the shape requires " + n);
    }
    var elems = futhark_from_data(info.elemtype, prim, data);
    var bytes = new Uint8Array(elems.buffer, elems.byteOffset, elems.byteLength);
    var p = this._malloc(bytes.length);
    try {
      this.wasm.HEAPU8.set(bytes, p);
      var ptr = this.wasm["_" + info.ops.new](this.ctx, p, ...shape);
      if (ptr == 0) {
        throw this._error();
      }
      return new FutharkArray(this, type, ptr);
    } finally {
      this.wasm._free(p);
    }
  }

  async _project(type, field, obj) {
    this._check_usable();
    futhark_check_value(obj, FutharkOpaque, this, type, "The record");
    return await this._call_with_output(field.project, field.type, [obj.ptr], () => {});
  }

  async _call_entry(name, info, args) {
    this._check_usable();
    if (args.length != info.inputs.length) {
      throw new TypeError("Entry point " + name + " expects " + info.inputs.length +
                          " arguments, but got " + args.length);
    }
    var c_args = info.inputs.map((input, i) =>
      this._to_c(input.type, args[i], "Argument " + i + " of entry point " + name));
    info.inputs.forEach((input, i) => {
      if (input.consumed && args.some((x, j) => j != i && x === args[i])) {
        throw new Error("Argument " + i + " of entry point " + name +
                        " is consumed, so it must not also be passed as another argument");
      }
    });
    var mark_consumed = () => info.inputs.forEach((input, i) => {
      if (input.consumed) {
        args[i].consumed = true;
      }
    });
    return await this._call_with_output(info.cfun, info.output.type, c_args, mark_consumed);
  }
}

// End of wrappers.js
