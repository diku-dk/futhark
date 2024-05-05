// Start of server_ws.js

// TODO: Really need to make the naming and casing in here consistent

// TODO: These are mostly just for debugging
async function newBuffer(fut, data) {
  const buf = fut.malloc(data.byteLength);
  const view = fut.m.HEAP8.subarray(buf, buf + data.byteLength);
  view.set(new Int8Array(data.buffer));

  const arr = await fut.new_i32_1d(buf, data.length);
  fut.free(buf);
  return arr;
}

async function getBufferValues(fut, buf) {
  const shape = fut.shape_i32_1d(buf);
  const len = fut.m.HEAP32[shape / 4];
  const vals = fut.malloc(len * 4);
  await fut.values_i32_1d(buf, vals);
  await fut.context_sync();
  return [vals, fut.m.HEAP32.subarray(vals/4, vals/4 + len)];
}

class BrowserServer {
  constructor(fut, port) {
    this.fut = fut;
    this.vars = {};

    this.commands = {
      'entry_points': this.cmd_entry_points.bind(this),
      'inputs': this.cmd_inputs.bind(this),
      'outputs': this.cmd_outputs.bind(this),
      'restore': this.cmd_restore.bind(this),
      'store': this.cmd_store.bind(this),
      'free': this.cmd_free.bind(this),
      'call': this.cmd_call.bind(this),
      'clear': this.cmd_clear.bind(this),
      'report': this.cmd_report.bind(this),
      'pause_profiling': this.cmd_pause_profiling.bind(this),
      'unpause_profiling': this.cmd_unpause_profiling.bind(this),
    };

    this.socket = new WebSocket("ws://" + window.location.host + "/ws");
    this.socket.onmessage = async (event) => {
      const msg = JSON.parse(event.data);
      console.log("WS command:", msg);

      let resp = undefined;
      try {
        if (!(msg.cmd in this.commands)) {
          throw "Unknown command: " + msg.cmd;
        }

        const fun = this.commands[msg.cmd];
        const res = await fun(...msg.args);

        await this.fut.context_sync();

        if (typeof res == "string") {
          resp = { status: "ok", text: res };
        }
        else {
          res['status'] = "ok";
          resp = res;
        }
      } catch (ex) {
        console.log(ex);
        resp = { status: "fail", text: ex.toString() };
      }
      this.socket.send(JSON.stringify(resp));
    };
    console.log("Created WS client.");
  }

  get_entry_point(entry) {
    if (entry in this.fut.entry_points) {
      return this.fut.entry_points[entry];
    }
    throw "Unknown entry point: " + entry;
  }

  get_manifest_entry_point(entry) {
    if (entry in this.fut.manifest.entry_points) {
      return this.fut.manifest.entry_points[entry];
    }
    throw "Unknown entry point: " + entry;
  }

  get_manifest_type(type) {
    if (type in this.fut.manifest.types) {
      return this.fut.manifest.types[type];
    }
    throw "Unknown type: " + type;
  }

  fut_function(fullName) {
    const name = fullName.replace("futhark_", "");
    let fun = this.fut[name];
    return fun.bind(this.fut);
  }

  check_var(name) {
    if (!(name in this.vars)) {
      throw "Unknown variable: " + name;
    }
  }

  set_var(name, val, typ) {
    this.vars[name] = {val: val, typ: typ};
  }

  get_var(name) {
    this.check_var(name);
    return this.vars[name];
  }

  delete_var(name) {
    delete this.vars[name];
  }

  async cmd_entry_points() {
    const entries = Object.keys(this.fut.entry_points);
    return entries.join("\n");
  }

  async cmd_inputs(entry) {
    const entry_info = this.get_manifest_entry_point(entry);
    const inputs = entry_info.inputs.map(function(arg) {
      if (arg.unique) { return "*" + arg.type; }
      return arg.type;
    });
    return inputs.join("\n");
  }

  async cmd_outputs(entry) {
    const entry_info = this.get_manifest_entry_point(entry);
    const outputs = entry_info.outputs.map(function(arg) {
      if (arg.unique) { return "*" + arg.type; }
      return arg.type;
    });
    return outputs.join("\n");
  }

  async cmd_restore(data, ...varsAndTypes) {
    for (let i = 0; i < data.length; i++) {
      // TODO: This only works for 1d arrays
      const name = varsAndTypes[i*2];
      const type = varsAndTypes[i*2 + 1];
      const type_info = this.get_manifest_type(type);
      const new_fun = this.fut_function(type_info.ops.new)

      // TODO: 32-bit int specific
      const len = data[i].length;
      const buf = this.fut.malloc(len * 4);
      for (let j = 0; j < len; j++) {
        this.fut.m.HEAP32[buf / 4 + j] = data[i][j];
      }

      const val = await new_fun(buf, BigInt(len));
      this.set_var(name, val, type);

      this.fut.free(buf);
    }

    return "";
  }

  async cmd_store(...vars) {
    let data = [];
    let types = [];
    for (const name of vars) {
      const {val, typ} = this.get_var(name);
      const type_info = this.get_manifest_type(typ);
      //const shape_fun = this.fut_function(type_info.ops.shape);
      const values_fun = this.fut_function(type_info.ops.values + "_js");

      // TODO: This really needs to be abstracted, only works for []i32
      //const shape = shape_fun(val);
      //const len = this.fut.m.HEAP32[shape / 4];
      //const buf = this.fut.malloc(len * 4);
      //await values_fun(val, buf);
      //await this.fut.context_sync();
      const values = await values_fun(val);

      //data.push(Array.from(this.fut.m.HEAP32.subarray(buf/4, buf/4 + len)));
      data.push(values);
      types.push(typ);

      //this.fut.free(buf);
    }

    const data_strings = data.map((a) => "[" + a.toString() + "]");
    return {'data': data_strings, 'types': types};
  }

  async cmd_free(name) {
    const {val, typ} = this.get_var(name);
    const type_info = this.get_manifest_type(typ);
    const free_fun = this.fut_function(type_info.ops.free);
    await free_fun(val);
    this.delete_var(name);
    return "";
  }

  async cmd_call(entry, ...outsAndIns) {
    const entry_info = this.get_manifest_entry_point(entry);
    const entry_fun = this.get_entry_point(entry);
    const outCount = entry_info.outputs.length;
    const outNames = outsAndIns.slice(0, outCount);
    const inNames = outsAndIns.slice(outCount, outsAndIns.length);
    const ins = inNames.map((n) => this.get_var(n).val);

    const startTime = performance.now();
    const outs = await entry_fun(...ins);
    await this.fut.context_sync();
    const endTime = performance.now();

    for (let i = 0; i < outNames.length; i++) {
      // TODO: This assumes that size of the value is 4 bytes
      const outVal = this.fut.m.HEAP32[outs[i] / 4];
      this.set_var(outNames[i], outVal, entry_info.outputs[i].type);
      this.fut.free(outs[i]);
    }

    return "runtime: " + Math.round((endTime - startTime) * 1000).toString();
  }

  async cmd_clear() {
    await this.fut.clear_caches();
    return "";
  }

  async cmd_report() {
    return await this.fut.report();
  }

  async cmd_pause_profiling() {
    await this.fut.pause_profiling();
    return "";
  }

  async cmd_unpause_profiling() {
    await this.fut.unpause_profiling();
    return "";
  }
}

async function runServer() {
  const m = await Module();
  const fut = new FutharkModule();
  await fut.init(m);

  //const inData = new Int32Array([1,2,3,4,5,6,7,8,9,10]);
  //const input = await newBuffer(fut, inData);

  //const [inValPtr, inVals] = await getBufferValues(fut, input);
  //console.log("input: ", inVals, " at ", inValPtr);

  //const [outPtrPtr] = await fut.entry_main(input);

  //const output = m.HEAP32[outPtrPtr / 4];
  //const [outValPtr, outVals] = await getBufferValues(fut, output);
  //console.log("output: ", outVals, " at ", outValPtr);

  //
  //fut.free(inValPtr);
  //fut.free(outValPtr);
  //fut.free(outPtrPtr);
  //await fut.free_i32_1d(input);
  //await fut.free_i32_1d(output);
 
  const server = new BrowserServer(fut);
}

runServer();

// End of server_ws.js
