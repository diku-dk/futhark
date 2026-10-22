import http from "http";
import fs from "fs";
import path from "path";
import { chromium } from "playwright";

const ROOT = process.cwd();

function contentType(filePath) {
  if (filePath.endsWith(".js")) return "application/javascript";
  if (filePath.endsWith(".mjs")) return "application/javascript";
  if (filePath.endsWith(".wasm")) return "application/wasm";
  if (filePath.endsWith(".json")) return "application/json";
  if (filePath.endsWith(".html")) return "text/html";
  return "application/octet-stream";
}

function startServer(html) {
  const server = http.createServer((req, res) => {
    const url = new URL(req.url, "http://localhost");

    res.setHeader("Cross-Origin-Opener-Policy", "same-origin");
    res.setHeader("Cross-Origin-Embedder-Policy", "require-corp");

    if (url.pathname === "/" || url.pathname === "/test.html") {
      res.setHeader("Content-Type", "text/html");
      res.end(html);
      return;
    }

    const filePath = path.join(ROOT, decodeURIComponent(url.pathname));

    if (!filePath.startsWith(ROOT)) {
      res.statusCode = 403;
      res.end("Forbidden");
      return;
    }

    if (!fs.existsSync(filePath) || !fs.statSync(filePath).isFile()) {
      res.statusCode = 404;
      res.end(`Not found: ${url.pathname}`);
      return;
    }

    res.setHeader("Content-Type", contentType(filePath));
    fs.createReadStream(filePath).pipe(res);
  });

  return new Promise((resolve) => {
    server.listen(0, "127.0.0.1", () => {
      const address = server.address();
      resolve({ server, port: address.port });
    });
  });
}

export async function runWebGPUTest({ name, html }) {
  const { server, port } = await startServer(html);

  const browser = await chromium.launch({
    headless: true,
    args: [
      "--no-sandbox",
      "--headless=new",
      "--enable-unsafe-webgpu",
      "--ignore-gpu-blocklist",
      "--enable-features=Vulkan,WebGPU",
      "--use-angle=vulkan",
    ],
  });

  try {
    const page = await browser.newPage();

    page.on("console", (msg) => {
      console.log(`[browser:${msg.type()}] ${msg.text()}`);
    });

    page.on("pageerror", (err) => {
      console.error("[pageerror]", err);
    });

    await page.goto(`http://127.0.0.1:${port}/test.html`);

    await page.waitForFunction(
      () =>
        window.__TEST_STATUS === "done" ||
        window.__TEST_STATUS === "failed" ||
        window.__TEST_STATUS === "skipped",
      undefined,
      { timeout: 30000 }
    );

    const result = await page.evaluate(() => ({
      status: window.__TEST_STATUS,
      error: window.__TEST_ERROR,
    }));

    if (result.status !== "done") {
      throw new Error(`${name} failed: ${result.error}`);
    }

    console.log();
    console.log(`${name} tests complete`);
    console.log();
  } finally {
    await browser.close();
    server.close();
  }
}