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
    this.shape = shape;
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

      const wasm_data = fut.malloc(data.byteLength);
      const wasm_view = fut.m.HEAPU8.subarray(wasm_data, wasm_data + data.byteLength);
      wasm_view.set(new Uint8Array(data.buffer, data.byteOffset, data.byteLength));

      const new_fun = wasm_fun(type_info.ops.new);
      const arr = new_fun(fut.ctx, wasm_data, ...shape);

      fut.free(wasm_data);

      return new FutharkArrayImpl(arr, shape);
    }

    get_shape() { return this.shape; }

    async values() { 
      futhark_assert(this.arr != undefined, "array already freed");

      const flat_len = Number(this.shape.reduce((a, b) => a * b));
      const flat_size = flat_len * prim_info.size;
      const wasm_data = fut.malloc(flat_size);

      await fut.m.ccall(type_info.ops.values,
        'number', ['number', 'number', 'number'],
        [fut.ctx, this.arr, wasm_data],
        {async: true});

      const data = prim_info.create_array(
        prim_info.get_heap(fut.m)
          .subarray(wasm_data / prim_info.size,
                    wasm_data / prim_info.size + flat_len)
      );

      fut.free(wasm_data);

      return data;
    };

    free() {
      const free_fun = wasm_fun(type_info.ops.free);
      free_fun(fut.ctx, this.arr);
      this.arr = undefined;
    }
  };
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
          real_inputs.push(inputs[i]);
        }
      }
      else {
        throw new Error("Unknown input type");
      }
    }

    let out_ptrs = [];
    for (let i = 0; i < entry_info.outputs.length; i++) {
      out_ptrs.push(fut.malloc(4));
    }

    await fut.m.ccall(entry_info.cfun, 'number',
      Array(1 + out_ptrs.length + real_inputs.length).fill('number'),
      [fut.ctx].concat(out_ptrs).concat(real_inputs), {async: true});

    let outputs = [];
    for (let i = 0; i < out_ptrs.length; i++) {
      const out_info = entry_info.outputs[i];
      if (out_info.type in primInfos) {
        const prim_info = primInfos[out_info.type];
        const val = prim_info.get_heap(fut.m)[out_ptrs[i] / prim_info.size];
        outputs.push(val);
      }
      else if (out_info.type in fut.manifest.types) {
        const type_info = fut.manifest.types[out_info.type];
        if (type_info.kind == "array") {
          const array_type = fut.types[out_info.type];
          const val = array_type.from_native(fut.m.HEAP32[out_ptrs[i] / 4]);
          outputs.push(val);
        }
        else {
          outputs.push(val);
        }
      }
      else {
        throw new Error("Unknown output type");
      }
    }

    for (const ptr of out_ptrs) {
      fut.free(ptr);
    }

    return outputs;
  };
}

// End of wrappers.js
