// Start of server_ws.js

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
    if (entry in this.fut.entry) {
      return this.fut.entry[entry];
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
    const entries = Object.keys(this.fut.available_entry_points);
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

  async cmd_restore(data_b64, ...varsAndTypes) {
    const data = Uint8Array.from(atob(data_b64), c => c.charCodeAt(0));
    const reader = new FutharkReader(data);

    for (let i = 0; i < varsAndTypes.length; i += 2) {
      const name = varsAndTypes[i];
      const type = varsAndTypes[i+1];

      const raw_val = reader.read_value(type);

      let val = undefined;
      if (type in this.fut.manifest.types) {
        const type_info = this.get_manifest_type(type);
        futhark_assert(type_info.kind == "array");

        const [data, shape] = raw_val;
        val = this.fut.types[type].from_data(data, ...shape);
      }
      else {
        // Scalar.
        val = raw_val;
      }

      this.set_var(name, val, type);
    }

    return "";
  }

  async cmd_store(...vars) {
    let data = "";
    let types = [];
    for (const name of vars) {
      const {val, typ} = this.get_var(name);

      let to_write = undefined;
      if (typ in this.fut.manifest.types) {
        const type_info = this.get_manifest_type(typ);
        futhark_assert(type_info.kind == "array");

        const values = await val.values();
        const shape = val.get_shape();
        to_write = [values, shape];
      }
      else {
        // Scalar.
        to_write = val;
      }

      const encoded = new FutharkWriter().encode_value(to_write, typ);
      data += btoa(String.fromCharCode.apply(null, encoded));

      types.push(typ);
    }

    return {'data': data, 'types': types};
  }

  async cmd_free(name) {
    const {val, typ} = this.get_var(name);
    if (val instanceof FutharkArray) {
      val.free();
    }
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
      this.set_var(outNames[i], outs[i], entry_info.outputs[i].type);
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

  // Setting fut into the global scope makes debugging a bit easier, and this is
  // not intended to be embedded into anything other than the internal
  // `futhark test` / `futhark bench` support anyway.
  window.fut = new FutharkModule();
  await fut.init(m);

  window.server = new BrowserServer(fut);
}

runServer();

// End of server_ws.js
