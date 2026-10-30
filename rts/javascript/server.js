// Start of server.js

// The server is implemented directly on top of the C API exported by the
// WebAssembly module, much like the C server.  The manifest describes the
// types and entry points, and names the C functions that operate on them.
//
// Calls into C go through 'ccall' with the async option, because in the
// WebGPU backend a C function may suspend while it waits for the GPU (via
// Asyncify), and 'ccall' then returns a promise of the result.  In modules
// without Asyncify, the option has no effect.  Command output is collected by
// '_print' and written out by the caller.  File access is also asynchronous,
// as a subclass may override it (see rts/webgpu/server_ws.js).
//
// A variable is a pair of a type name and a value.  A value of primitive type
// is a JavaScript number, or a BigInt for 64-bit integers.  Booleans are 0 or
// 1, and f16 values are their bit patterns.  A value of any other type is a
// pointer to the C object.

var serverPrimtypes = {
  'i8':   { size: 1, get: (dv, p) => dv.getInt8(p),           set: (dv, p, x) => dv.setInt8(p, x) },
  'i16':  { size: 2, get: (dv, p) => dv.getInt16(p, true),    set: (dv, p, x) => dv.setInt16(p, x, true) },
  'i32':  { size: 4, get: (dv, p) => dv.getInt32(p, true),    set: (dv, p, x) => dv.setInt32(p, x, true) },
  'i64':  { size: 8, get: (dv, p) => dv.getBigInt64(p, true), set: (dv, p, x) => dv.setBigInt64(p, x, true) },
  'u8':   { size: 1, get: (dv, p) => dv.getUint8(p),          set: (dv, p, x) => dv.setUint8(p, x) },
  'u16':  { size: 2, get: (dv, p) => dv.getUint16(p, true),   set: (dv, p, x) => dv.setUint16(p, x, true) },
  'u32':  { size: 4, get: (dv, p) => dv.getUint32(p, true),   set: (dv, p, x) => dv.setUint32(p, x, true) },
  'u64':  { size: 8, get: (dv, p) => dv.getBigUint64(p, true), set: (dv, p, x) => dv.setBigUint64(p, x, true) },
  'f16':  { size: 2, get: (dv, p) => dv.getUint16(p, true),   set: (dv, p, x) => dv.setUint16(p, x, true) },
  'f32':  { size: 4, get: (dv, p) => dv.getFloat32(p, true),  set: (dv, p, x) => dv.setFloat32(p, x, true) },
  'f64':  { size: 8, get: (dv, p) => dv.getFloat64(p, true),  set: (dv, p, x) => dv.setFloat64(p, x, true) },
  'bool': { size: 1, get: (dv, p) => dv.getUint8(p),          set: (dv, p, x) => dv.setUint8(p, x ? 1 : 0) }
};

// Pointers and size_t are 32 bits in WebAssembly.
var serverPointer = {
  size: 4,
  get: (dv, p) => dv.getUint32(p, true),
  set: (dv, p, x) => dv.setUint32(p, x, true)
};

class Server {

  constructor(ctx, manifest) {
    this.ctx = ctx;
    this.wasm = ctx.wasm;
    this.manifest = manifest;
    this._vars = {};
    this._output = [];
  }

  _print(line) {
    this._output.push(line);
  }

  _get_arg(args, i) {
    if (i < args.length) {
      return args[i];
    } else {
      throw 'Insufficient command args';
    }
  }

  _get_entry_point(entry) {
    if (entry in this.manifest.entry_points) {
      return this.manifest.entry_points[entry];
    } else {
      throw "Unknown entry point: " + entry;
    }
  }

  _check_var(vname) {
    if (!(vname in this._vars)) {
      throw 'Unknown variable: ' + vname;
    }
  }

  _check_new_var(vname) {
    if (vname in this._vars) {
      throw 'Variable already exists: ' + vname;
    }
  }

  _get_var(vname) {
    this._check_var(vname);
    return this._vars[vname];
  }

  _get_typed_var(vname, tname) {
    var v = this._get_var(vname);
    if (v.type !== tname) {
      throw "Variable " + vname + " has type " + v.type + ", expected " + tname;
    }
    return v.value;
  }

  _set_var(vname, tname, value) {
    this._vars[vname] = { type: tname, value: value };
  }

  _parse_ints(args, n) {
    if (args.length != n) {
      throw n + " integers expected but " + args.length + " provided.";
    }
    return args.map((x) => {
      if (!/^[0-9]+$/.test(x)) {
        throw "Invalid integer: " + x;
      }
      return BigInt(x);
    });
  }

  // Calling into the C API.

  async _c(fname, ...args) {
    return await this.wasm.ccall(fname, 'number',
      Array(1 + args.length).fill('number'),
      [this.ctx.ctx, ...args], {async: true});
  }

  // Read a NUL-terminated string allocated by C, and free it.
  _c_string(p) {
    var heap = this.wasm.HEAPU8;
    var str = new TextDecoder().decode(heap.subarray(p, heap.indexOf(0, p)));
    this.wasm._free(p);
    return str;
  }

  _get_error() {
    return this._c_string(this.wasm._futhark_context_get_error(this.ctx.ctx));
  }

  _check(err) {
    if (err != 0) {
      throw this._get_error();
    }
  }

  async _sync() {
    this._check(await this._c('futhark_context_sync'));
  }

  _malloc(n) {
    // Avoid zero-sized allocations, which may return NULL.
    return this.wasm._malloc(Math.max(n, 1));
  }

  _view() {
    // Must be recreated on every use, as the memory may have grown.
    return new DataView(this.wasm.HEAPU8.buffer);
  }

  _repr(tname) {
    return tname in serverPrimtypes ? serverPrimtypes[tname] : serverPointer;
  }

  _sizeof(tname) {
    return this._repr(tname).size;
  }

  _load(tname, p) {
    return this._repr(tname).get(this._view(), p);
  }

  _poke(tname, p, x) {
    this._repr(tname).set(this._view(), p, x);
  }

  // Call 'f' with a pointer to space for one value of the given type, and
  // return the value that it stores there.
  async _with_out(tname, f) {
    var p = this._malloc(this._sizeof(tname));
    try {
      await f(p);
      return this._load(tname, p);
    } finally {
      this.wasm._free(p);
    }
  }

  _bytes(p, n) {
    return this.wasm.HEAPU8.slice(p, p + n);
  }

  async _read_file(fname) {
    return new Uint8Array(require('fs').readFileSync(fname));
  }

  async _write_file(fname, bytes) {
    require('fs').writeFileSync(fname, bytes);
  }

  // Types.

  _type(tname) {
    if (tname in this.manifest.types) {
      return this.manifest.types[tname];
    } else if (tname in serverPrimtypes) {
      return null;
    } else {
      throw "Unknown type: " + tname;
    }
  }

  _kind(tname) {
    var t = this._type(tname);
    if (t === null) {
      return "primitive";
    } else if (t.kind == "array" || t.opaque_array || t.record_array) {
      return "array";
    } else if (t.record) {
      return "record";
    } else if (t.sum) {
      return "sum";
    } else {
      return "opaque";
    }
  }

  // Information about an array type.  Transparent arrays have 'ops', and
  // opaque arrays have the functions directly.
  _array_type(tname) {
    if (this._kind(tname) != "array") {
      throw "Not an array type";
    }
    var t = this._type(tname);
    if (t.kind == "array") {
      return { rank: t.rank, elemtype: t.elemtype, ops: t.ops };
    } else {
      return t.opaque_array || t.record_array;
    }
  }

  _record_type(tname) {
    if (this._kind(tname) != "record") {
      throw "Not a record type";
    }
    return this._type(tname).record;
  }

  _sum_type(tname) {
    if (this._kind(tname) != "sum") {
      throw "Not a sum type";
    }
    return this._type(tname).sum;
  }

  async _free_value(tname, value) {
    var t = this._type(tname);
    if (t !== null) {
      this._check(await this._c(t.ops.free, value));
    }
  }

  async _shape(a, arr) {
    var fshape = a.ops ? a.ops.shape : a.shape;
    var p = await this._c(fshape, arr);
    var shape = [];
    for (var i = 0; i < a.rank; i++) {
      shape.push(this._load('i64', p + i * 8));
    }
    return shape;
  }

  _check_bounds(shape, is) {
    for (var i = 0; i < shape.length; i++) {
      if (is[i] >= shape[i]) {
        throw "Index " + is.join(",") + " out of bounds for shape " + shape.join(",");
      }
    }
  }

  // Values.

  async _restore_value(reader, tname) {
    var t = this._type(tname);
    if (t === null) {
      return read_value(tname, reader);
    } else if (t.kind == "array") {
      var [shape, data] = read_value(tname, reader);
      var bytes = new Uint8Array(data.buffer, data.byteOffset, data.byteLength);
      var p = this._malloc(bytes.length);
      try {
        this.wasm.HEAPU8.set(bytes, p);
        var arr = await this._c(t.ops.new, p, ...shape);
        if (arr == 0) {
          throw this._get_error();
        }
        await this._sync();
        return arr;
      } finally {
        this.wasm._free(p);
      }
    } else {
      // As in the C server, we pass all the remaining input to the restore
      // function, then ask how large the object is in serialised form.
      var buff = reader.get_buff();
      var p = this._malloc(buff.length);
      try {
        this.wasm.HEAPU8.set(buff, p);
        var obj = await this._c(t.ops.restore, p);
        if (obj == 0) {
          throw this._get_error();
        }
      } finally {
        this.wasm._free(p);
      }
      var n = await this._with_out('size', async (np) => {
        this._check(await this._c(t.ops.store, obj, 0, np));
      });
      reader.buff = buff.slice(n);
      return obj;
    }
  }

  async _store_value(tname, value) {
    var t = this._type(tname);
    if (t === null) {
      var p = this._malloc(this._sizeof(tname));
      try {
        this._poke(tname, p, value);
        return binary_value(tname, [], this._bytes(p, this._sizeof(tname)));
      } finally {
        this.wasm._free(p);
      }
    } else if (t.kind == "array") {
      var shape = await this._shape(this._array_type(tname), value);
      var n = shape.reduce((x, y) => x * y, 1n) * BigInt(this._sizeof(t.elemtype));
      var p = this._malloc(Number(n));
      try {
        this._check(await this._c(t.ops.values, value, p));
        await this._sync();
        return binary_value(t.elemtype, shape, this._bytes(p, Number(n)));
      } finally {
        this.wasm._free(p);
      }
    } else {
      var pp = this._malloc(serverPointer.size * 2);
      var np = pp + serverPointer.size;
      try {
        this._poke('ptr', pp, 0);
        this._check(await this._c(t.ops.store, value, pp, np));
        await this._sync();
        var p = this._load('ptr', pp);
        var bytes = this._bytes(p, this._load('size', np));
        this.wasm._free(p);
        return bytes;
      } finally {
        this.wasm._free(pp);
      }
    }
  }

  // Commands.

  _cmd_inputs(args) {
    var entry = this._get_entry_point(this._get_arg(args, 0));
    for (var input of entry.inputs) {
      this._print((input.consumed ? "*" : "") + input.type);
    }
  }

  _cmd_output(args) {
    var entry = this._get_entry_point(this._get_arg(args, 0));
    this._print((entry.output.fresh ? "*" : "") + entry.output.type);
  }

  async _cmd_clear(args) {
    this._check(await this._c('futhark_context_clear_caches'));
  }

  async _cmd_pause_profiling(args) {
    await this._c('futhark_context_pause_profiling');
  }

  async _cmd_unpause_profiling(args) {
    await this._c('futhark_context_unpause_profiling');
  }

  async _cmd_report(args) {
    this._print(this._c_string(await this._c('futhark_context_report')));
  }

  async _cmd_free(args) {
    for (var vname of args) {
      var v = this._get_var(vname);
      await this._free_value(v.type, v.value);
      delete this._vars[vname];
    }
  }

  _cmd_rename(args) {
    var oldname = this._get_arg(args, 0);
    var newname = this._get_arg(args, 1);
    var v = this._get_var(oldname);
    this._check_new_var(newname);
    this._vars[newname] = v;
    delete this._vars[oldname];
  }

  _cmd_types(args) {
    for (var t in serverPrimtypes) {
      this._print(t);
    }
    for (var t in this.manifest.types) {
      this._print(t);
    }
  }

  _cmd_entry_points(args) {
    for (var e in this.manifest.entry_points) {
      this._print(e);
    }
  }

  _cmd_attributes(args) {
    var entry = this._get_entry_point(this._get_arg(args, 0));
    for (var attr of entry.attributes) {
      this._print(attr);
    }
  }

  async _cmd_call(args) {
    var entry = this._get_entry_point(this._get_arg(args, 0));
    var out_vname = this._get_arg(args, 1);
    var in_vnames = args.slice(2);
    if (in_vnames.length != entry.inputs.length) {
      throw "Invalid argument count, expected " + entry.inputs.length;
    }
    var ins = in_vnames.map((v, i) => this._get_typed_var(v, entry.inputs[i].type));
    this._check_new_var(out_vname);

    var bef = performance.now() * 1000;
    var out = await this._with_out(entry.output.type, async (p) => {
      this._check(await this._c(entry.cfun, p, ...ins));
      await this._sync();
    });
    var aft = performance.now() * 1000;
    this._print("runtime: " + Math.round(aft - bef));
    this._set_var(out_vname, entry.output.type, out);
  }

  async _cmd_store(args) {
    var fname = this._get_arg(args, 0);
    var bufs = [];
    for (var vname of args.slice(1)) {
      var v = this._get_var(vname);
      bufs.push(await this._store_value(v.type, v.value));
    }
    await this._write_file(fname, concat_bytes(bufs));
  }

  async _cmd_restore(args) {
    var fname = this._get_arg(args, 0);
    if (args.length % 2 == 0) {
      throw "Invalid argument count";
    }

    var reader = new Reader(await this._read_file(fname));
    for (var i = 1; i < args.length; i += 2) {
      var vname = args[i];
      var tname = args[i + 1];
      this._check_new_var(vname);
      this._type(tname);
      try {
        this._set_var(vname, tname, await this._restore_value(reader, tname));
      } catch (err) {
        throw "Failed to restore variable " + vname + ".\nPossibly malformed data in " + fname + ".\n" + err.toString();
      }
    }
    skip_spaces(reader);
    if (reader.get_buff().length != 0) {
      throw "Expected EOF after reading values";
    }
  }

  _cmd_kind(args) {
    this._print(this._kind(this._get_arg(args, 0)));
  }

  _cmd_type(args) {
    this._print(this._get_var(this._get_arg(args, 0)).type);
  }

  _cmd_rank(args) {
    this._print(this._array_type(this._get_arg(args, 0)).rank);
  }

  _cmd_elemtype(args) {
    this._print(this._array_type(this._get_arg(args, 0)).elemtype);
  }

  async _cmd_shape(args) {
    var v = this._get_var(this._get_arg(args, 0));
    for (var d of await this._shape(this._array_type(v.type), v.value)) {
      this._print(d.toString());
    }
  }

  async _cmd_new_array(args) {
    var dst = this._get_arg(args, 0);
    var tname = this._get_arg(args, 1);
    this._check_new_var(dst);
    var a = this._array_type(tname);
    var dims = this._parse_ints(args.slice(2, 2 + a.rank), a.rank);
    var vnames = args.slice(2 + a.rank);
    var n = dims.reduce((x, y) => x * y, 1n);
    if (BigInt(vnames.length) != n) {
      throw "Expected " + n + " values, but got " + vnames.length + ".";
    }
    var vs = vnames.map((v) => this._get_typed_var(v, a.elemtype));

    var size = this._sizeof(a.elemtype);
    var p = this._malloc(vs.length * size);
    try {
      vs.forEach((v, i) => this._poke(a.elemtype, p + i * size, v));
      var arr;
      if (a.ops) {
        arr = await this._c(a.ops.new, p, ...dims);
        if (arr == 0) {
          throw this._get_error();
        }
      } else {
        arr = await this._with_out(tname, async (out) => {
          this._check(await this._c(a.new, out, p, ...dims));
        });
      }
      await this._sync();
      this._set_var(dst, tname, arr);
    } finally {
      this.wasm._free(p);
    }
  }

  async _cmd_set(args) {
    var arr = this._get_var(this._get_arg(args, 0));
    var a = this._array_type(arr.type);
    var val = this._get_typed_var(this._get_arg(args, 1), a.elemtype);
    var is = this._parse_ints(args.slice(2), a.rank);
    this._check_bounds(await this._shape(a, arr.value), is);
    var fset = a.ops ? a.ops.set : a.set;
    this._check(await this._c(fset, arr.value, val, ...is));
    await this._sync();
  }

  async _cmd_index(args) {
    var dst = this._get_arg(args, 0);
    var arr = this._get_var(this._get_arg(args, 1));
    this._check_new_var(dst);
    var a = this._array_type(arr.type);
    var is = this._parse_ints(args.slice(2), a.rank);
    this._check_bounds(await this._shape(a, arr.value), is);
    var findex = a.ops ? a.ops.index : a.index;
    var v = await this._with_out(a.elemtype, async (p) => {
      this._check(await this._c(findex, p, arr.value, ...is));
      await this._sync();
    });
    this._set_var(dst, a.elemtype, v);
  }

  async _cmd_zip(args) {
    var dst = this._get_arg(args, 0);
    var tname = this._get_arg(args, 1);
    this._check_new_var(dst);
    var a = this._array_type(tname);
    if (!a.fields) {
      throw "Cannot zip this array type";
    }
    var vnames = args.slice(2);
    if (vnames.length != a.fields.length) {
      throw a.fields.length + " arrays expected but " + vnames.length + " values provided.";
    }
    var vs = vnames.map((v, i) => this._get_typed_var(v, a.fields[i].type));
    var arr = await this._with_out(tname, async (p) => {
      this._check(await this._c(a.zip, p, ...vs));
      await this._sync();
    });
    this._set_var(dst, tname, arr);
  }

  async _cmd_unzip(args) {
    var arr = this._get_var(this._get_arg(args, 0));
    var a = this._array_type(arr.type);
    if (!a.fields) {
      throw "Cannot unzip this array type";
    }
    var dsts = args.slice(1);
    if (dsts.length != a.fields.length) {
      throw a.fields.length + " arrays expected but " + dsts.length + " values provided.";
    }
    dsts.forEach((dst) => this._check_new_var(dst));
    for (var i = 0; i < a.fields.length; i++) {
      var f = a.fields[i];
      var v = await this._with_out(f.type, async (p) => {
        this._check(await this._c(f.project, p, arr.value));
      });
      this._set_var(dsts[i], f.type, v);
    }
    await this._sync();
  }

  _cmd_fields(args) {
    for (var f of this._record_type(this._get_arg(args, 0)).fields) {
      this._print(f.name + " " + f.type);
    }
  }

  async _cmd_new(args) {
    var dst = this._get_arg(args, 0);
    var tname = this._get_arg(args, 1);
    this._check_new_var(dst);
    var r = this._record_type(tname);
    var vnames = args.slice(2);
    if (vnames.length != r.fields.length) {
      throw r.fields.length + " fields expected but " + vnames.length + " values provided.";
    }
    var vs = vnames.map((v, i) => this._get_typed_var(v, r.fields[i].type));
    var obj = await this._with_out(tname, async (p) => {
      this._check(await this._c(r.new, p, ...vs));
    });
    this._set_var(dst, tname, obj);
  }

  async _cmd_project(args) {
    var dst = this._get_arg(args, 0);
    var from = this._get_var(this._get_arg(args, 1));
    var field = this._get_arg(args, 2);
    this._check_new_var(dst);
    var f = this._record_type(from.type).fields.find((f) => f.name == field);
    if (f === undefined) {
      throw "No such field: " + field;
    }
    var v = await this._with_out(f.type, async (p) => {
      this._check(await this._c(f.project, p, from.value));
    });
    this._set_var(dst, f.type, v);
  }

  _cmd_variants(args) {
    for (var v of this._sum_type(this._get_arg(args, 0)).variants) {
      this._print(v.name);
      for (var t of v.payload) {
        this._print("- " + t);
      }
    }
  }

  async _variant_of(v) {
    var s = this._sum_type(v.type);
    return s.variants[await this._c(s.variant, v.value)];
  }

  async _cmd_variant(args) {
    this._print((await this._variant_of(this._get_var(this._get_arg(args, 0)))).name);
  }

  async _cmd_construct(args) {
    var dst = this._get_arg(args, 0);
    var tname = this._get_arg(args, 1);
    var vname = this._get_arg(args, 2);
    this._check_new_var(dst);
    var variant = this._sum_type(tname).variants.find((v) => v.name == vname);
    if (variant === undefined) {
      throw "No such variant: " + vname;
    }
    var vnames = args.slice(3);
    if (vnames.length != variant.payload.length) {
      throw variant.payload.length + " values expected but " + vnames.length + " provided.";
    }
    var vs = vnames.map((v, i) => this._get_typed_var(v, variant.payload[i]));
    var obj = await this._with_out(tname, async (p) => {
      this._check(await this._c(variant.construct, p, ...vs));
    });
    this._set_var(dst, tname, obj);
  }

  async _cmd_destruct(args) {
    var v = this._get_var(this._get_arg(args, 0));
    var variant = await this._variant_of(v);
    var dsts = args.slice(1);
    if (dsts.length != variant.payload.length) {
      throw variant.payload.length + " variables expected but " + dsts.length + " provided.";
    }
    dsts.forEach((dst) => this._check_new_var(dst));
    var ps = variant.payload.map((t) => this._malloc(this._sizeof(t)));
    try {
      this._check(await this._c(variant.destruct, ...ps, v.value));
      variant.payload.forEach((t, i) => this._set_var(dsts[i], t, this._load(t, ps[i])));
    } finally {
      ps.forEach((p) => this.wasm._free(p));
    }
  }

  async _process_command(cmd, args) {
    switch (cmd) {
    case 'inputs': return this._cmd_inputs(args);
    case 'output': return this._cmd_output(args);
    case 'call': return this._cmd_call(args);
    case 'restore': return this._cmd_restore(args);
    case 'store': return this._cmd_store(args);
    case 'free': return this._cmd_free(args);
    case 'clear': return this._cmd_clear(args);
    case 'pause_profiling': return this._cmd_pause_profiling(args);
    case 'unpause_profiling': return this._cmd_unpause_profiling(args);
    case 'report': return this._cmd_report(args);
    case 'rename': return this._cmd_rename(args);
    case 'types': return this._cmd_types(args);
    case 'entry_points': return this._cmd_entry_points(args);
    case 'attributes': return this._cmd_attributes(args);
    case 'kind': return this._cmd_kind(args);
    case 'type': return this._cmd_type(args);
    case 'rank': return this._cmd_rank(args);
    case 'elemtype': return this._cmd_elemtype(args);
    case 'shape': return this._cmd_shape(args);
    case 'new_array': return this._cmd_new_array(args);
    case 'set': return this._cmd_set(args);
    case 'index': return this._cmd_index(args);
    case 'zip': return this._cmd_zip(args);
    case 'unzip': return this._cmd_unzip(args);
    case 'fields': return this._cmd_fields(args);
    case 'new': return this._cmd_new(args);
    case 'project': return this._cmd_project(args);
    case 'variants': return this._cmd_variants(args);
    case 'construct': return this._cmd_construct(args);
    case 'destruct': return this._cmd_destruct(args);
    case 'variant': return this._cmd_variant(args);
    default: throw "Unknown command: " + cmd;
    }
  }

  async _process_line(line) {
    var words = split_words(line);
    if (words.length == 0) {
      throw "Empty line";
    }
    await this._process_command(words[0], words.slice(1));
  }

  async run() {
    console.log('%%% OK'); // TODO figure out if flushing is neccesary for JS
    const readline = require('readline');
    const rl = readline.createInterface(process.stdin);
    // Commands are asynchronous, so we must finish one before reading the
    // next.
    for await (const line of rl) {
      if (line == "") {
        break;
      }
      this._output = [];
      try {
        await this._process_line(line);
        this._output.forEach((l) => console.log(l));
        console.log('%%% OK');
      } catch (err) {
        console.log('%%% FAILURE');
        this._output.forEach((l) => console.log(l));
        console.log(err);
        console.log('%%% OK');
      }
    }
    process.exit(0);
  }
}

// Split a command line into words separated by whitespace.  A word may be
// enclosed in double quotes, in which case it may contain whitespace.
function split_words(line) {
  var words = [];
  var re = /"([^"]*)"|[^\s"]+/g;
  var m;
  var pos = 0;
  while ((m = re.exec(line)) !== null) {
    if (line.slice(pos, m.index).trim() != "") {
      throw "Unterminated quote";
    }
    words.push(m[1] !== undefined ? m[1] : m[0]);
    pos = re.lastIndex;
  }
  if (line.slice(pos).trim() != "") {
    throw "Unterminated quote";
  }
  return words;
}

// End of server.js
