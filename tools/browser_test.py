#!/usr/bin/env python3

import argparse
import base64
import json
import shlex
import subprocess
import sys
import os
from io import BytesIO

import asyncio
import aiohttp
from aiohttp import web
import numpy as np

from selenium import webdriver

parser = argparse.ArgumentParser()
parser.add_argument(
    "program", help="the program to wrap (without .js or other file extension)"
)
parser.add_argument(
    "--no-server-proxy",
    help=(
        "only act as HTTP server, do not proxy Futhark server protocol\n"
        "(implies --no-browser)"
    ),
    action="store_true",
)
parser.add_argument(
    "--no-browser",
    help=(
        "do not start a browser, instead wait for one to connect.\n"
        "Can also be set via NO_BROWSER=1 env variable."
    ),
    action="store_true",
)
parser.add_argument(
    "--show-browser",
    help=(
        "disable headless mode for browser.\n"
        "Can also be set via HEADLESS=0 env variable."
    ),
    action="store_true",
)
parser.add_argument(
    "--web-driver",
    help=(
        "URL of a remote WebDriver to connec to.\n"
        "Can also be set via WEB_DRIVER_URL env variable."
    ),
)
parser.add_argument(
    "--log",
    help=(
        "log file for debug output.\n"
        "Can also be set via LOG_FILE env variable."
    ),
)
args = parser.parse_args()

program_name = args.program
if program_name.startswith("./"):
    program_name = program_name[2:]
script_name = program_name + ".js"
wasm_name = program_name + ".wasm"
wasm_map_name = program_name + ".wasm.map"
source_name = program_name + ".c"

log_path = os.environ.get("LOG_FILE")
if log_path is None:
    log_path = args.log

log_file = None
if log_path is not None:
    log_file = open(log_path, "w")

remote_driver_url = os.environ.get("WEB_DRIVER_URL")
if remote_driver_url is None:
    remote_driver_url = args.web_driver

no_browser_env = os.environ.get("NO_BROWSER")
no_browser = None
if no_browser_env == "0":
    no_browser = False
elif no_browser_env == "1":
    no_browser = True
elif no_browser is None:
    no_browser = args.no_browser

headless_env = os.environ.get("HEADLESS")
headless = None
if headless_env == "0":
    headless = False
elif headless_env == "1":
    headless = True
elif headless is None:
    headless = not args.show_browser


def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, flush=True, **kwargs)
    if log_file is not None:
        print(*args, file=log_file, flush=True, **kwargs)


index_page = (
    f"<!DOCTYPE html>\n"
    f"<html>\n"
    f"<head>\n"
    f"   <title>futhark test</title>\n"
    f'   <script src="{script_name}"></script>\n'
    f"</head>\n"
    f"<body>\n"
    f"   <h1>futhark test</h1>\n"
    f"</body>\n"
    f"</html>"
)

default_headers = {
    # This makes the site an "isolated context", most
    # notably improving timing measurement resolution
    # from 100 microseconds to 5 microseconds.
    "Cross-Origin-Opener-Policy": "same-origin",
    "Cross-Origin-Embedder-Policy": "require-corp",
}


async def handle_index(request):
    return web.Response(
        text=index_page, content_type="text/html", headers=default_headers
    )


async def handle_file(request):
    file = request.rel_url.path.lstrip("/")

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

    return web.Response(
        body=contents, content_type=content_type, headers=default_headers
    )


def wrap_restore(args):
    fname = args[0]

    with open(fname, "rb") as f:
        data = f.read()

    data = base64.b64encode(data).decode("utf-8")

    return [data] + args[1:]


def wrap_store_resp(fname, resp):
    data = base64.b64decode(resp["data"].encode("utf-8"))
    with open(fname, "wb") as f:
        f.write(data)
    return ""


async def handle_ws(request):
    ws = web.WebSocketResponse(max_msg_size=2**30)
    await ws.prepare(request)

    toWS = request.app["toWS"]
    toStdIO = request.app["toStdIO"]

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

        await ws.send_json({"cmd": cmd, "args": args})
        msg = await ws.receive()

        if msg.type == aiohttp.WSMsgType.ERROR:
            eprint("ws connection closed with exception %s" % ws.exception())
            break

        resp = json.loads(msg.data)
        eprint("Got response:", resp)

        text = ""
        if cmd == "store" and resp["status"] == "ok":
            text = wrap_store_resp(orig_args[0], resp)
        else:
            text = resp["text"]

        toStdIO.put_nowait((resp["status"], text))

    if not ws.closed:
        await ws.close()

    eprint("WS connection closed, stopping server")
    app["stop"].set()


def start_browser():
    options = webdriver.ChromeOptions()

    if headless:
        options.add_argument("--headless=new")

    if remote_driver_url is not None:
        driver = webdriver.Remote(
            command_executor=remote_driver_url, options=options
        )
    else:
        # Need these extra options when running properly on Linux, but
        # specifying them on Windows is not allowed. For now, just assume a
        # remote driver will run on Windows and a local one on Linux, this check
        # might need to be adjusted if we ever use remote drivers where the
        # remote is also Linux.
        options.add_argument("--enable-unsafe-webgpu")
        options.add_argument("--enable-features=Vulkan")

        if headless:
            # https://developer.chrome.com/blog/supercharge-web-ai-testing#enable-webgpu
            options.add_argument("--no-sandbox")
            options.add_argument("--use-angle=vulkan")
            options.add_argument("--disable-vulkan-surface")

        driver = webdriver.Chrome(options=options)

    loop = asyncio.get_running_loop()
    get_task = loop.run_in_executor(None, driver.get, "http://localhost:8100")

    return driver, get_task


def stop_browser(driver):
    driver.quit()


async def start_server(app, toWS, toStdIO):
    app["toWS"] = toWS
    app["toStdIO"] = toStdIO

    app["stop"] = asyncio.Event()

    runner = web.AppRunner(app)
    await runner.setup()
    site = web.TCPSite(runner, "0.0.0.0", 8100)

    await site.start()

    if not no_browser:
        driver, get_task = start_browser()

    await app["stop"].wait()
    if not no_browser:
        await get_task
    await runner.cleanup()

    if not no_browser:
        stop_browser(driver)


app = web.Application()
app.add_routes(
    [
        web.get("/", handle_index),
        web.get("/index.html", handle_index),
        web.get(f"/{script_name}", handle_file),
        web.get(f"/{wasm_name}", handle_file),
        web.get(f"/{wasm_map_name}", handle_file),
        web.get(f"/{source_name}", handle_file),
        web.get("/ws", handle_ws),
    ]
)


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
    toWS = asyncio.Queue()
    toStdIO = asyncio.Queue()
    if args.no_server_proxy:
        eprint(f"Wrapping {script_name}; only providing web server")
        eprint("Hosting at 0.0.0.0:8100")
        await start_server(app, toWS, toStdIO)
    else:
        eprint(f"Wrapping {script_name}; proxying the Futhark server protocol")
        await asyncio.gather(
            start_server(app, toWS, toStdIO), handle_stdio(toWS, toStdIO)
        )


asyncio.run(main())
