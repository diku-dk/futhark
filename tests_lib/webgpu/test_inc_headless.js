import { runWebGPUTest } from "./webgpu_test_harness.mjs";

const html = String.raw`
<!doctype html>
<html>
  <head>
    <meta charset="utf-8">
    <title>WebGPU inc test</title>
  </head>
  <body>
    <script src="/build/inc/inc.js"></script>

    <script>
      function section(title) {
        console.log("");
        console.log("== " + title + " ==");
      }

      function ok(message) {
        console.log("  ✓ " + message);
      }

      function assert(condition, message) {
        if (!condition) {
          throw new Error(message || "Assertion failed");
        }
      }

      function assertEqual(actual, expected, message) {
        if (actual !== expected) {
          throw new Error(message || ("Expected " + expected + ", got " + actual));
        }
      }
        

      window.__TEST_STATUS = "running";

      (async function main() {
        try {
          console.log("");
          console.log("Testing inc.fut");

          section("Environment");

          assert(typeof navigator !== "undefined" && !!navigator.gpu,
            "navigator.gpu should exist");

          ok("WebGPU is available");

          section("Preferred FutharkModule API");

          const module = await Module();
          const fut = new FutharkModule();

          assert(fut instanceof FutharkModule, "fut should be a FutharkModule");

          await fut.init(module);

          assert(fut.entry, "fut.entry should exist");
          assert(typeof fut.entry.inc === "function", "fut.entry.inc should be a function");

          const module_api_res = await fut.entry.inc(9);
          assertEqual(module_api_res, 10);

          ok("FutharkModule supports init(module) and entry aliases");

          section("Shared utility methods");

          assert(typeof fut.context_sync === "function", "context_sync should exist");
          assert(typeof fut.clear_caches === "function", "clear_caches should exist");
          assert(typeof fut.report === "function", "report should exist");
          assert(typeof fut.pause_profiling === "function", "pause_profiling should exist");
          assert(typeof fut.unpause_profiling === "function", "unpause_profiling should exist");

          ok("context_sync(), clear_caches(), report(), pause_profiling(), and unpause_profiling() are exposed");
          
          section("Cleanup");

          fut.free();

          ok("context freed");

          window.__TEST_STATUS = "done";
        } catch (e) {
          window.__TEST_STATUS = "failed";
          window.__TEST_ERROR = e && e.stack ? e.stack : String(e);
          console.error(window.__TEST_ERROR);
        }
      })();
    </script>
  </body>
</html>
`;

await runWebGPUTest({ name: "inc", html });