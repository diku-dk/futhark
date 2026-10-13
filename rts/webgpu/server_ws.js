// Start of server_ws.js

// The server used by `futhark test` and `futhark bench`.  It can run in two
// ways:
//
// * In Node, with WebGPU provided by Dawn, as with tools/webgpu/node-runner.sh.
//   It then speaks the server protocol on its standard input and output, just
//   like the WASM server.
//
// * In a browser, where commands arrive over a WebSocket from
//   tools/browser_test.py, which speaks the actual server protocol on its
//   standard input and output.  Files are then transferred through the proxy:
//   files to restore are fetched from it, and stored values are sent back to it
//   as base64 in the response, for it to write to the file.
//
// The commands are implemented by the Server class from
// rts/javascript/server.js.  The difference is that every call into C is
// asynchronous, as WebGPU operations may suspend the module (via Asyncify).

// https://stackoverflow.com/a/66046176/3112547
async function bufferToBase64(buffer) {
  const base64url = await new Promise(r => {
    const reader = new FileReader();
    reader.onload= () => r(reader.result);
    reader.readAsDataURL(new Blob([buffer]));
  });
  return base64url.slice(base64url.indexOf(',') + 1);
}

class WebGPUServer extends Server {
  constructor(fut) {
    super({ wasm: fut.m, ctx: fut.ctx }, fut.manifest);
  }

  async _c(fname, ...args) {
    return await this.wasm.ccall(fname, 'number',
      Array(1 + args.length).fill('number'),
      [this.ctx.ctx, ...args], {async: true});
  }
}

class BrowserServer extends WebGPUServer {
  constructor(fut) {
    super(fut);
    this._stored = [];

    this.socket = new WebSocket("ws://" + window.location.host + "/ws");
    this.socket.onmessage = async (event) => {
      const msg = JSON.parse(event.data);
      console.log("WS command:", msg);

      this._output = [];
      this._stored = [];
      let resp = undefined;
      try {
        await this._process_command(msg.cmd, msg.args);
        resp = { status: "ok", text: this._output.join("\n") };
        if (this._stored.length > 0) {
          resp.data = await bufferToBase64(concat_bytes(this._stored));
        }
      } catch (ex) {
        console.log(ex);
        resp = { status: "fail", text: this._output.concat([ex.toString()]).join("\n") };
      }
      this.socket.send(JSON.stringify(resp));
    };
    console.log("Created WS client.");
  }

  async _read_file(fname) {
    const response = await fetch(fname);
    if (!response.ok) {
      throw "Failed to fetch file: " + response.statusText;
    }
    return await response.bytes();
  }

  async _write_file(fname, bytes) {
    this._stored.push(bytes);
  }
}

async function runServer() {
  const fut = new FutharkModule();
  await fut.init(await Module());

  if (typeof window === 'undefined') {
    await new WebGPUServer(fut).run();
  } else {
    // Setting these in the global scope makes debugging a bit easier, and this
    // is not intended to be embedded into anything other than the internal
    // `futhark test` / `futhark bench` support anyway.
    window.fut = fut;
    window.server = new BrowserServer(fut);
  }
}

runServer();

// End of server_ws.js
