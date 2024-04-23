#!/usr/bin/env python

import sys

import asyncio
import aiohttp
from aiohttp import web

program_name = sys.argv[1]
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

    async for msg in ws:
        if msg.type == aiohttp.WSMsgType.ERROR:
            eprint("ws connection closed with exception %s" % ws.exception())
        elif msg.type == aiohttp.WSMsgType.TEXT:
            eprint("ws data: %s" % msg.data)
            await ws.send_str("test reply")

    eprint("ws connection closed")

eprint("Wrapping", script_name)
eprint("Source name is", source_name)
app = web.Application()
app.add_routes(
    [web.get('/', handle_index),
     web.get('/index.html', handle_index),
     web.get(f'/{script_name}', handle_file),
     web.get(f'/{wasm_name}', handle_file),
     web.get(f'/{wasm_map_name}', handle_file),
     web.get(f'/{source_name}', handle_file),
     web.get('/ws', handle_ws)])
web.run_app(app, port=8100)

#asyncio.run(main())
