// Start of wrappers.js

// All of the functionality is in subclasses for the individual array types,
// which are generated into fields of the FutharkModule class
// (e.g. `fut.i32_1d` if `fut` is the FutharkModule instance).
// This is just used as a marker so we can check if some object is an instance
// of any of those generated classes.
class FutharkArray {
  constructor(name, arr, shape) {
    // Name is only for debugging since the debugger will show
    // 'FutharkArrayImpl' as type for all array types.
    this.type_name = name;
    this.arr = arr;
    this.shape_data = shape;
  }

  validCheck() {
    if (this.arr === undefined) {
      throw new Error("Using freed memory");
    }
  }

  futharkType() {
    return this.type_name;
  }

  shape() {
    this.validCheck();
    return this.shape_data;
  }
}

function make_array_class(fut, name) {
  const type_info = fut.manifest.types[name];
  const prim_info = primInfos[type_info.elemtype];

  function wasm_fun(full_name) {
    const name = "_" + full_name;
    return fut.m[name];
  }

  return class FutharkArrayImpl extends FutharkArray {
    constructor(arr, shape) {
      super(name, arr, shape);
    }

    static from_native(arr) {
      const shape_fun = wasm_fun(type_info.ops.shape);
      const shape_ptr = shape_fun(fut.ctx, arr);

      const shape = new BigInt64Array(
        fut.m.HEAP64.subarray(shape_ptr / 8, shape_ptr / 8 + type_info.rank));

      return new FutharkArrayImpl(arr, shape);
    }

    static from_data(data, ...shape) {
      futhark_assert(shape.length == type_info.rank, "wrong number of shape arguments");
      if (typeof(shape[0]) === 'number') {
        shape = BigInt64Array.from(shape.map((x) => BigInt(x)));
      }

      if (data instanceof Array) {
        data = prim_info.create_array(data);
      }
      futhark_assert(data instanceof prim_info.array_type,
        "expected Array or correct TypedArray");

      const wasm_data = fut.m._malloc(data.byteLength);
      const wasm_view = fut.m.HEAPU8.subarray(wasm_data, wasm_data + data.byteLength);
      wasm_view.set(new Uint8Array(data.buffer, data.byteOffset, data.byteLength));

      const new_fun = wasm_fun(type_info.ops.new);
      const arr = new_fun(fut.ctx, wasm_data, ...shape);

      fut.m._free(wasm_data);

      return new FutharkArrayImpl(arr, shape);
    }

    // get_shape() { return this.shape; }

    async toTypedArray() {
      return await this.values();
    }

    async toArray() {
      const dims = this.shape();
      const ta = await this.values();

      return (function nest(offs, ds) {
        const d0 = Number(ds[0]);

        if (ds.length === 1) {
          return Array.from(ta.subarray(offs, offs + d0));
        } else {
          const stride = Number(ds.slice(1).reduce((a, b) => a * b, 1n));
          return Array.from(Array(d0), (_, i) => nest(offs + i * stride, ds.slice(1)));
        }
      })(0, dims);
    }

    async values() {
      this.validCheck();

      const shape = this.shape();
      const flat_len = Number(shape.reduce((a, b) => a * b));
      const flat_size = flat_len * prim_info.size;
      const wasm_data = fut.m._malloc(flat_size);

      await fut.m.ccall(type_info.ops.values,
        'number', ['number', 'number', 'number'],
        [fut.ctx, this.arr, wasm_data],
        {async: true});

      const data = prim_info.create_array(
        prim_info.get_heap(fut.m)
          .subarray(wasm_data / prim_info.size,
                    wasm_data / prim_info.size + flat_len)
      );

      fut.m._free(wasm_data);

      return data;
    };

    free() {
      this.validCheck();

      const free_fun = wasm_fun(type_info.ops.free);
      free_fun(fut.ctx, this.arr);
      this.arr = undefined;
    }
  };
}

// Like FutharkArray, this is a marker class for the classes generated for
// the individual opaque types.
class FutharkOpaque {
  constructor(name, ptr) {
    // Name is only for debugging, as for FutharkArray.
    this.type_name = name;
    this.ptr = ptr;
  }

  validCheck() {
    if (this.ptr === undefined) {
      throw new Error("Using freed memory");
    }
  }

  futharkType() {
    return this.type_name;
  }
}

function make_opaque_class(fut, name) {
  const type_info = fut.manifest.types[name];

  return class FutharkOpaqueImpl extends FutharkOpaque {
    constructor(ptr) {
      super(name, ptr);
    }

    static from_native(ptr) {
      return new FutharkOpaqueImpl(ptr);
    }

    free() {
      this.validCheck();
      fut.m["_" + type_info.ops.free](fut.ctx, this.ptr);
      this.ptr = undefined;
    }
  };
}

// Read the value of the given type that a C function has stored at the
// pointer 'p'.
function read_output(fut, type, p) {
  if (type in primInfos) {
    const prim_info = primInfos[type];
    return prim_info.get_heap(fut.m)[p / prim_info.size];
  }
  if (!(type in fut.types)) {
    throw new Error("Unknown output type");
  }
  return fut.types[type].from_native(fut.m.HEAP32[p / 4]);
}

// Asyncify resumes a suspended C function by calling it again without
// arguments, which are then ignored, as the function restores its state from
// the Asyncify stack.  However, WebAssembly cannot convert the missing argument
// for an int64_t parameter to a BigInt, so resuming an entry point that takes
// an i64 fails.  We remember the arguments of the most recent 'ccall', which is
// the one in flight (Asyncify allows only one), and resume with those instead.
// This relies on all calls into C going through 'ccall', and on all arguments
// passed being 'number' (so 'ccall' passes them unchanged).
function fix_asyncify_rewind(m) {
  const ccall = m.ccall;
  let args_in_flight = [];
  m.ccall = (cfun, ret, types, args, opts) => {
    args_in_flight = args;
    return ccall(cfun, ret, types, args, opts);
  };
  m.Asyncify.doRewind = (ptr) =>
    m.Asyncify.funcWrappers.get(m.Asyncify.getDataRewindFunc(ptr))(...args_in_flight);
}

// Call a C function that stores a value of the given type through its first
// argument (after the context), and return that value.
async function call_with_output(fut, cfun, type, args) {
  // Large enough for any primitive value or pointer.
  const out = fut.m._malloc(8);
  try {
    await fut.m.ccall(cfun, 'number',
      Array(2 + args.length).fill('number'),
      [fut.ctx, out].concat(args), {async: true});
    return read_output(fut, type, out);
  } finally {
    fut.m._free(out);
  }
}

function make_entry_function(fut, name) {
  const entry_info = fut.manifest.entry_points[name];

  return async function(...inputs) {
    futhark_assert(inputs.length == entry_info.inputs.length,
      "Unexpected number of input arguments");

    let real_inputs = [];

    for (let i = 0; i < inputs.length; i++) {
      const typ = entry_info.inputs[i].type;
      if (typ in primInfos) {
        real_inputs.push(primInfos[typ].scalar_type(inputs[i]));
      }
      else if (typ in fut.manifest.types) {
        const type_info = fut.manifest.types[typ];
        if (type_info.kind == "array") {
          if (!(inputs[i] instanceof FutharkArray)) {
            throw new Error("Entry point array arguments must be FutharkArrays");
          }
          real_inputs.push(inputs[i].arr);
        }
        else {
          if (!(inputs[i] instanceof FutharkOpaque)) {
            throw new Error("Entry point opaque arguments must be FutharkOpaques");
          }
          real_inputs.push(inputs[i].ptr);
        }
      }
      else {
        throw new Error("Unknown input type");
      }
    }

    return await call_with_output(fut, entry_info.cfun, entry_info.output.type, real_inputs);
  };
}

// A function that projects a field of a record, given as a FutharkOpaque.
function make_project_function(fut, field) {
  return async function(obj) {
    if (!(obj instanceof FutharkOpaque)) {
      throw new Error("Projection argument must be a FutharkOpaque");
    }
    obj.validCheck();
    return await call_with_output(fut, field.project, field.type, [obj.ptr]);
  };
}

// Populate the types, entry points, and record projections of a FutharkModule
// from its manifest.  As in the WASM backend, each array type is also
// available as e.g. 'fut.i32_1d', and the projection function for a record
// field as e.g. 'fut.project_opaque_foo_x'.
function init_wrappers(fut) {
  for (const [name, type_info] of Object.entries(fut.manifest.types)) {
    if (type_info.kind == "array") {
      const cls = make_array_class(fut, name);
      fut.types[name] = cls;
      fut[type_info.elemtype + "_" + type_info.rank + "d"] = cls;
    } else {
      fut.types[name] = make_opaque_class(fut, name);
      if (type_info.record) {
        for (const field of type_info.record.fields) {
          fut[field.project.replace(/^futhark_/, "")] = make_project_function(fut, field);
        }
      }
    }
  }

  for (const name of Object.keys(fut.manifest.entry_points)) {
    fut.entry[name] = make_entry_function(fut, name);
  }
}

// End of wrappers.js
