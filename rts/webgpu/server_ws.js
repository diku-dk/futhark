// Start of server_ws.js

class BrowserServer {
  constructor(ctx, port) {
    this.ctx = ctx;
    this.vars = {};

    this.socket = new WebSocket("ws://" + window.location.host + "/ws");
    this.socket.onopen = (event) => {
      this.socket.send("A test message");
    }
    this.socket.onmessage = (event) => {
      console.log(event.data);
    };
    console.log("Created server");
  }

  check_var(name) {
    if (!(name in this.vars)) {
      throw 'Unknown variable: ' + name;
    }
  }

  set_var(name, val, typ) {
    this.vars[name] = {val: val, typ: typ};
  }

  get_type(name) {
    this.check_var(name);
    return this.vars[name].typ;
  }

  get_var(name) {
    this.check_var(name);
    return this.vars[name].val;
  }
}

async function getBufferValues(m, ctx, buf) {
  const shape = m.futhark_shape_i32_1d(ctx, buf);
  const len = m.HEAP32[shape / 4];
  const vals = m.malloc(len * 4);
  await m.futhark_values_i32_1d(ctx, buf, vals);
  await m.futhark_context_sync(ctx);

  return [vals, m.HEAP32.subarray(vals/4, vals/4 + len)];
}

async function runServer() {
  const m = await Module();

  const cfg = m.futhark_context_config_new();
  const ctx = await m.futhark_context_new(cfg);
  
  const inData = new Int32Array([1,2,3,4,5,6,7,8,9,10]);
  const inBuf = new Uint8Array(inData.buffer);
  const input = await m.futhark_new_i32_1d(ctx, inBuf, inData.length);

  const [inValPtr, inVals] = await getBufferValues(m, ctx, input);
  console.log("input: ", inVals, " at ", inValPtr);

  const outPtrPtr = m.malloc(4);

  await m.futhark_entry_main(ctx, outPtrPtr, input);

  const output = m.HEAP32[outPtrPtr / 4];
  const [outValPtr, outVals] = await getBufferValues(m, ctx, output);
  console.log("output: ", outVals, " at ", outValPtr);

  const server = new BrowserServer(ctx);
  
  m.free(inValPtr);
  m.free(outValPtr);
  m.free(outPtrPtr);
  await m.futhark_free_i32_1d(ctx, input);
  await m.futhark_free_i32_1d(ctx, output);
}

runServer();

// End of server_ws.js
