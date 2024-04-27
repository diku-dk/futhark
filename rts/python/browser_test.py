#!/usr/bin/env python

import json
import shlex
import sys
from io import BytesIO

import asyncio
import aiohttp
from aiohttp import web

import numpy as np
from values import ReaderInput, read_value, construct_binary_value

program_name = sys.argv[1]
serve_only = len(sys.argv) > 2 and sys.argv[2] == "--serve-only"

if program_name.startswith("./"):
    program_name = program_name[2:]

#log_path = sys.argv[3] if len(sys.argv) > 3 and sys.argv[2] == "--log" else None
log_path = "./test_log"

log_file = None
if log_path is not None:
    log_file = open(log_path, "w")

script_name = program_name + ".js"
wasm_name = program_name + ".wasm"
wasm_map_name = program_name + ".wasm.map"
source_name = program_name + ".c"

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, flush=True, **kwargs)
    if log_file is not None:
        print(*args, file=log_file, flush=True, **kwargs)

index_page = (f"<!DOCTYPE html>\n"
              f"<html>\n"
              f"<head>\n"
              f"   <title>futhark test</title>\n"
              f"   <script src=\"{script_name}\"></script>\n"
              f"</head>\n"
              f"<body>\n"
              f"   <h1>futhark test</h1>\n"
              f"</body>\n"
              f"</html>"
             )

async def handle_index(request):
    return web.Response(text=index_page, content_type="text/html")

async def handle_file(request):
    file = request.rel_url.path.lstrip('/')

    content_type = "text/plain"
    if file.endswith(".js"):
        content_type = "text/javascript"
    elif file.endswith(".wasm"):
        content_type = "application/wasm"
    elif file.endswith(".map"):
        content_type = "application/json"

    contents = b""
    with open(file, "rb") as f:
        contents = f.read()

    return web.Response(body=contents, content_type=content_type)

def restore_val(reader, typename):
    # TODO: This ignores opaque types
    return read_value(typename, reader)

def wrap_restore(args):
    orig_args = args
    fname = args[0]
    args = args[1:]

    data = list()

    # TODO: This ignores a lot of error handling right now.
    with open(fname, "rb") as f:
        reader = ReaderInput(f)
        while args != []:
            typename = args[1]
            args = args[2:]
            val = restore_val(reader, typename)
            data.append(val.tolist())

    return [data] + orig_args[1:]

def store_val(f, val):
    f.write(construct_binary_value(val))

def wrap_store_resp(fname, resp):
    types = resp['types']
    data_strings = resp['data']
    with open(fname, "wb") as f:
        for (typ, text) in zip(types, data_strings):
            reader = ReaderInput(BytesIO(text.encode('utf-8')))
            val = read_value(typ, reader)
            store_val(f, val)
    return ""

async def handle_ws(request):
    ws = web.WebSocketResponse()
    await ws.prepare(request)

    toWS = request.app['toWS']
    toStdIO = request.app['toStdIO']

    # Notify that we have a browser connected
    toStdIO.put_nowait("connected")

    while True:
        cmd, args = await toWS.get()

        if cmd == "close":
            break

        orig_args = args
        if cmd == "restore":
            args = wrap_restore(args)
        elif cmd == "store":
            args = args[1:]

        await ws.send_json({ "cmd": cmd, "args": args })
        msg = await ws.receive()

        if msg.type == aiohttp.WSMsgType.ERROR:
            eprint("ws connection closed with exception %s" % ws.exception())
            break

        resp = json.loads(msg.data)
        eprint("Got response:", resp)

        text = ""
        if cmd == "store":
            text = wrap_store_resp(orig_args[0], resp)
        else:
            text = resp['text']

        toStdIO.put_nowait((resp['status'], text))

    if not ws.closed:
        await ws.close()

    eprint("WS connection closed, stopping server")
    app['stop'].set()

async def start_server(app, toWS, toStdIO):
    app['toWS'] = toWS
    app['toStdIO'] = toStdIO

    app['stop'] = asyncio.Event()

    runner = web.AppRunner(app)
    await runner.setup()
    site = web.TCPSite(runner, '0.0.0.0', 8100)

    await site.start()
    await app['stop'].wait()
    await runner.cleanup()

app = web.Application()
app.add_routes(
    [web.get('/', handle_index),
     web.get('/index.html', handle_index),
     web.get(f'/{script_name}', handle_file),
     web.get(f'/{wasm_name}', handle_file),
     web.get(f'/{wasm_map_name}', handle_file),
     web.get(f'/{source_name}', handle_file),
     web.get('/ws', handle_ws)])

async def read_stdin_line():
    loop = asyncio.get_event_loop()
    return await loop.run_in_executor(None, sys.stdin.readline)

async def handle_stdio(toWS, toStdIO):
    # Wait for an initial signal that a web browser client has connected.
    await toStdIO.get()

    eprint("Browser client detected, starting Futhark server protocol")
    print("%%% OK", flush=True)

    while True:
        line = await read_stdin_line()
        eprint("Got line:", line.rstrip())

        if line.strip() == "":
            toWS.put_nowait(("close", []))
            break

        command, *args = shlex.split(line)

        toWS.put_nowait((command, args))
        status, text = await toStdIO.get()

        if status == "ok":
            print(text, flush=True)
            print("%%% OK", flush=True)
        else:
            print("%%% FAILURE", flush=True)
            print(text, flush=True)
            print("%%% OK", flush=True)

async def main():
    if serve_only:
        eprint(f"Wrapping {script_name}; only providing web server")
        eprint("Hosting at 0.0.0.0:8100")
        await start_server(app)
    else:
        eprint(f"Wrapping {script_name}; proxying the Futhark server protocol")
        toWS = asyncio.Queue()
        toStdIO = asyncio.Queue()
        await asyncio.gather(
                start_server(app, toWS, toStdIO),
                handle_stdio(toWS, toStdIO))
    
asyncio.run(main())
