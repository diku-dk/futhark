// Start of server.js

// The server is implemented directly on top of the C API exported by the
// WebAssembly module, much like the C server.  The manifest describes the
// types and entry points, and names the C functions that operate on them.
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

  _c(fname, ...args) {
    return this.wasm['_' + fname](this.ctx.ctx, ...args);
  }

  _check(err) {
    if (err != 0) {
      throw this.ctx.get_error();
    }
  }

  _sync() {
    this._check(this.wasm._futhark_context_sync(this.ctx.ctx));
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
  _with_out(tname, f) {
    var p = this._malloc(this._sizeof(tname));
    try {
      f(p);
      return this._load(tname, p);
    } finally {
      this.wasm._free(p);
    }
  }

  _bytes(p, n) {
    return Buffer.from(this.wasm.HEAPU8.slice(p, p + n));
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

  _free_value(tname, value) {
    var t = this._type(tname);
    if (t !== null) {
      this._check(this._c(t.ops.free, value));
    }
  }

  _shape(a, arr) {
    var fshape = a.ops ? a.ops.shape : a.shape;
    var p = this._c(fshape, arr);
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

  _restore_value(reader, tname) {
    var t = this._type(tname);
    if (t === null) {
      return read_value(tname, reader);
    } else if (t.kind == "array") {
      var [shape, data] = read_value(tname, reader);
      var bytes = new Uint8Array(data.buffer, data.byteOffset, data.byteLength);
      var p = this._malloc(bytes.length);
      try {
        this.wasm.HEAPU8.set(bytes, p);
        var arr = this._c(t.ops.new, p, ...shape);
        if (arr == 0) {
          throw this.ctx.get_error();
        }
        this._sync();
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
        var obj = this._c(t.ops.restore, p);
        if (obj == 0) {
          throw this.ctx.get_error();
        }
      } finally {
        this.wasm._free(p);
      }
      var n = this._with_out('size', (np) => {
        this._check(this._c(t.ops.store, obj, 0, np));
      });
      reader.buff = buff.slice(n);
      return obj;
    }
  }

  _store_value(tname, value) {
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
      var shape = this._shape(this._array_type(tname), value);
      var n = shape.reduce((x, y) => x * y, 1n) * BigInt(this._sizeof(t.elemtype));
      var p = this._malloc(Number(n));
      try {
        this._check(this._c(t.ops.values, value, p));
        this._sync();
        return binary_value(t.elemtype, shape, this._bytes(p, Number(n)));
      } finally {
        this.wasm._free(p);
      }
    } else {
      var pp = this._malloc(serverPointer.size * 2);
      var np = pp + serverPointer.size;
      try {
        this._poke('ptr', pp, 0);
        this._check(this._c(t.ops.store, value, pp, np));
        this._sync();
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
      console.log((input.consumed ? "*" : "") + input.type);
    }
  }

  _cmd_output(args) {
    var entry = this._get_entry_point(this._get_arg(args, 0));
    console.log((entry.output.fresh ? "*" : "") + entry.output.type);
  }

  _cmd_dummy(args) {
    // pass
  }

  _cmd_free(args) {
    for (var vname of args) {
      var v = this._get_var(vname);
      this._free_value(v.type, v.value);
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
      console.log(t);
    }
    for (var t in this.manifest.types) {
      console.log(t);
    }
  }

  _cmd_entry_points(args) {
    for (var e in this.manifest.entry_points) {
      console.log(e);
    }
  }

  _cmd_attributes(args) {
    var entry = this._get_entry_point(this._get_arg(args, 0));
    for (var attr of entry.attributes) {
      console.log(attr);
    }
  }

  _cmd_call(args) {
    var entry = this._get_entry_point(this._get_arg(args, 0));
    var out_vname = this._get_arg(args, 1);
    var in_vnames = args.slice(2);
    if (in_vnames.length != entry.inputs.length) {
      throw "Invalid argument count, expected " + entry.inputs.length;
    }
    var ins = in_vnames.map((v, i) => this._get_typed_var(v, entry.inputs[i].type));
    this._check_new_var(out_vname);

    var bef = performance.now() * 1000;
    var out = this._with_out(entry.output.type, (p) => {
      this._check(this._c(entry.cfun, p, ...ins));
      this._sync();
    });
    var aft = performance.now() * 1000;
    console.log("runtime: " + Math.round(aft - bef));
    this._set_var(out_vname, entry.output.type, out);
  }

  _cmd_store(args) {
    var fname = this._get_arg(args, 0);
    var bufs = [];
    for (var vname of args.slice(1)) {
      var v = this._get_var(vname);
      bufs.push(this._store_value(v.type, v.value));
    }
    require("fs").writeFileSync(fname, Buffer.concat(bufs));
  }

  _cmd_restore(args) {
    var fname = this._get_arg(args, 0);
    if (args.length % 2 == 0) {
      throw "Invalid argument count";
    }

    var reader = new Reader(fname);
    for (var i = 1; i < args.length; i += 2) {
      var vname = args[i];
      var tname = args[i + 1];
      this._check_new_var(vname);
      this._type(tname);
      try {
        this._set_var(vname, tname, this._restore_value(reader, tname));
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
    console.log(this._kind(this._get_arg(args, 0)));
  }

  _cmd_type(args) {
    console.log(this._get_var(this._get_arg(args, 0)).type);
  }

  _cmd_rank(args) {
    console.log(this._array_type(this._get_arg(args, 0)).rank);
  }

  _cmd_elemtype(args) {
    console.log(this._array_type(this._get_arg(args, 0)).elemtype);
  }

  _cmd_shape(args) {
    var v = this._get_var(this._get_arg(args, 0));
    for (var d of this._shape(this._array_type(v.type), v.value)) {
      console.log(d.toString());
    }
  }

  _cmd_new_array(args) {
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
        arr = this._c(a.ops.new, p, ...dims);
        if (arr == 0) {
          throw this.ctx.get_error();
        }
      } else {
        arr = this._with_out(tname, (out) => {
          this._check(this._c(a.new, out, p, ...dims));
        });
      }
      this._sync();
      this._set_var(dst, tname, arr);
    } finally {
      this.wasm._free(p);
    }
  }

  _cmd_set(args) {
    var arr = this._get_var(this._get_arg(args, 0));
    var a = this._array_type(arr.type);
    var val = this._get_typed_var(this._get_arg(args, 1), a.elemtype);
    var is = this._parse_ints(args.slice(2), a.rank);
    var shape = this._shape(a, arr.value);
    this._check_bounds(shape, is);
    if (a.ops) {
      var i = 0n;
      for (var j = 0; j < a.rank; j++) {
        i = i * shape[j] + is[j];
      }
      var p = this._c(a.ops.values_raw, arr.value);
      this._poke(a.elemtype, p + Number(i) * this._sizeof(a.elemtype), val);
    } else {
      this._check(this._c(a.set, arr.value, val, ...is));
      this._sync();
    }
  }

  _cmd_index(args) {
    var dst = this._get_arg(args, 0);
    var arr = this._get_var(this._get_arg(args, 1));
    this._check_new_var(dst);
    var a = this._array_type(arr.type);
    var is = this._parse_ints(args.slice(2), a.rank);
    this._check_bounds(this._shape(a, arr.value), is);
    var findex = a.ops ? a.ops.index : a.index;
    var v = this._with_out(a.elemtype, (p) => {
      this._check(this._c(findex, p, arr.value, ...is));
      this._sync();
    });
    this._set_var(dst, a.elemtype, v);
  }

  _cmd_zip(args) {
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
    var arr = this._with_out(tname, (p) => {
      this._check(this._c(a.zip, p, ...vs));
      this._sync();
    });
    this._set_var(dst, tname, arr);
  }

  _cmd_unzip(args) {
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
    a.fields.forEach((f, i) => {
      var v = this._with_out(f.type, (p) => {
        this._check(this._c(f.project, p, arr.value));
      });
      this._set_var(dsts[i], f.type, v);
    });
    this._sync();
  }

  _cmd_fields(args) {
    for (var f of this._record_type(this._get_arg(args, 0)).fields) {
      console.log(f.name + " " + f.type);
    }
  }

  _cmd_new(args) {
    var dst = this._get_arg(args, 0);
    var tname = this._get_arg(args, 1);
    this._check_new_var(dst);
    var r = this._record_type(tname);
    var vnames = args.slice(2);
    if (vnames.length != r.fields.length) {
      throw r.fields.length + " fields expected but " + vnames.length + " values provided.";
    }
    var vs = vnames.map((v, i) => this._get_typed_var(v, r.fields[i].type));
    var obj = this._with_out(tname, (p) => {
      this._check(this._c(r.new, p, ...vs));
    });
    this._set_var(dst, tname, obj);
  }

  _cmd_project(args) {
    var dst = this._get_arg(args, 0);
    var from = this._get_var(this._get_arg(args, 1));
    var field = this._get_arg(args, 2);
    this._check_new_var(dst);
    var f = this._record_type(from.type).fields.find((f) => f.name == field);
    if (f === undefined) {
      throw "No such field: " + field;
    }
    var v = this._with_out(f.type, (p) => {
      this._check(this._c(f.project, p, from.value));
    });
    this._set_var(dst, f.type, v);
  }

  _cmd_variants(args) {
    for (var v of this._sum_type(this._get_arg(args, 0)).variants) {
      console.log(v.name);
      for (var t of v.payload) {
        console.log("- " + t);
      }
    }
  }

  _variant_of(v) {
    var s = this._sum_type(v.type);
    return s.variants[this._c(s.variant, v.value)];
  }

  _cmd_variant(args) {
    console.log(this._variant_of(this._get_var(this._get_arg(args, 0))).name);
  }

  _cmd_construct(args) {
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
    var obj = this._with_out(tname, (p) => {
      this._check(this._c(variant.construct, p, ...vs));
    });
    this._set_var(dst, tname, obj);
  }

  _cmd_destruct(args) {
    var v = this._get_var(this._get_arg(args, 0));
    var variant = this._variant_of(v);
    var dsts = args.slice(1);
    if (dsts.length != variant.payload.length) {
      throw variant.payload.length + " variables expected but " + dsts.length + " provided.";
    }
    dsts.forEach((dst) => this._check_new_var(dst));
    var ps = variant.payload.map((t) => this._malloc(this._sizeof(t)));
    try {
      this._check(this._c(variant.destruct, ...ps, v.value));
      variant.payload.forEach((t, i) => this._set_var(dsts[i], t, this._load(t, ps[i])));
    } finally {
      ps.forEach((p) => this.wasm._free(p));
    }
  }

  _process_line(line) {
    var words = split_words(line);
    if (words.length == 0) {
      throw "Empty line";
    } else {
      var cmd = words[0];
      var args = words.slice(1);
      switch (cmd) {
      case 'inputs': this._cmd_inputs(args); break;
      case 'output': this._cmd_output(args); break;
      case 'call': this._cmd_call(args); break;
      case 'restore': this._cmd_restore(args); break;
      case 'store': this._cmd_store(args); break;
      case 'free': this._cmd_free(args); break;
      case 'clear': this._cmd_dummy(args); break;
      case 'pause_profiling': this._cmd_dummy(args); break;
      case 'unpause_profiling': this._cmd_dummy(args); break;
      case 'report': this._cmd_dummy(args); break;
      case 'rename': this._cmd_rename(args); break;
      case 'types': this._cmd_types(args); break;
      case 'entry_points': this._cmd_entry_points(args); break;
      case 'attributes': this._cmd_attributes(args); break;
      case 'kind': this._cmd_kind(args); break;
      case 'type': this._cmd_type(args); break;
      case 'rank': this._cmd_rank(args); break;
      case 'elemtype': this._cmd_elemtype(args); break;
      case 'shape': this._cmd_shape(args); break;
      case 'new_array': this._cmd_new_array(args); break;
      case 'set': this._cmd_set(args); break;
      case 'index': this._cmd_index(args); break;
      case 'zip': this._cmd_zip(args); break;
      case 'unzip': this._cmd_unzip(args); break;
      case 'fields': this._cmd_fields(args); break;
      case 'new': this._cmd_new(args); break;
      case 'project': this._cmd_project(args); break;
      case 'variants': this._cmd_variants(args); break;
      case 'construct': this._cmd_construct(args); break;
      case 'destruct': this._cmd_destruct(args); break;
      case 'variant': this._cmd_variant(args); break;
      default: throw "Unknown command: " + cmd;
      }
    }
  }

  run() {
    console.log('%%% OK'); // TODO figure out if flushing is neccesary for JS
    const readline = require('readline');
    const rl = readline.createInterface(process.stdin);
    rl.on('line', (line) => {
      if (line == "") {
        rl.close();
        return;
      }
      try {
        this._process_line(line);
        console.log('%%% OK');
      } catch (err) {
        console.log('%%% FAILURE');
        console.log(err);
        console.log('%%% OK');
      }
    }).on('close', () => { process.exit(0); });
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
