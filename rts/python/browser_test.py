#!/usr/bin/env python

import json
import shlex
import sys

import asyncio
import aiohttp
from aiohttp import web

program_name = sys.argv[1]
serve_only = len(sys.argv) > 2 and sys.argv[2] == "--serve-only"

script_name = program_name + ".js"
wasm_name = program_name + ".wasm"
wasm_map_name = program_name + ".wasm.map"
source_name = program_name + ".c"

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

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

async def handle_ws(request):
    ws = web.WebSocketResponse()
    await ws.prepare(request)

    toWS = request.app['toWS']
    toStdIO = request.app['toStdIO']

    # Notify that we have a browser connected
    toStdIO.put_nowait("connected")

    while True:
        cmd, args = await toWS.get()
        await ws.send_json({ "cmd": cmd, "args": args })
        msg = await ws.receive()

        if msg.type == aiohttp.WSMsgType.ERROR:
            eprint("ws connection closed with exception %s" % ws.exception())
            break

        resp = json.loads(msg.data)
        toStdIO.put_nowait((resp['status'], resp['text']))

    eprint("ws connection closed")

async def start_server(app, toWS, toStdIO):
    app['toWS'] = toWS
    app['toStdIO'] = toStdIO
    runner = web.AppRunner(app)
    await runner.setup()
    site = web.TCPSite(runner, '0.0.0.0', 8100)
    await site.start()
    
    while True:
        await asyncio.sleep(3600)

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
    while True:
        line = await read_stdin_line()
        command, *args = shlex.split(line)
        toWS.put_nowait((command, args))
        status, text = await toStdIO.get()
        if status == "ok":
            print(text)
            print("%%% OK")
        else:
            print("%%% FAILURE")
            print(text)
            print("%%% OK")

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
