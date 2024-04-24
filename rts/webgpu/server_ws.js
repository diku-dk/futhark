// Start of server_ws.js

class BrowserServer {
  constructor(fut, port) {
    this.fut = fut;
    this.vars = {};

    this.socket = new WebSocket("ws://" + window.location.host + "/ws");
    this.socket.onmessage = (event) => {
      const msg = JSON.parse(event.data);
      console.log(msg);
      this.socket.send(JSON.stringify({ status: "ok", text: "some output" }));
    };
    console.log("Created WS client.");
  }

  get_entry_point(entry) {
    if (entry in this.fut.entry_points) {
      return this.fut.entry_points[entry];
    }
    throw "Unknown entry point: " + entry;
  }

  check_var(name) {
    if (!(name in this.vars)) {
      throw "Unknown variable: " + name;
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

async function runServer() {
  const m = await Module();
  const fut = new FutharkModule();
  await fut.init(m);

  const inData = new Int32Array([1,2,3,4,5,6,7,8,9,10]);
  const input = await newBuffer(fut, inData);

  const [inValPtr, inVals] = await getBufferValues(fut, input);
  console.log("input: ", inVals, " at ", inValPtr);

  const [outPtrPtr] = await fut.entry_main(input);

  const output = m.HEAP32[outPtrPtr / 4];
  const [outValPtr, outVals] = await getBufferValues(fut, output);
  console.log("output: ", outVals, " at ", outValPtr);

  const server = new BrowserServer(fut);
  
  fut.free(inValPtr);
  fut.free(outValPtr);
  fut.free(outPtrPtr);
  await fut.free_i32_1d(input);
  await fut.free_i32_1d(output);
}

runServer();

// End of server_ws.js
