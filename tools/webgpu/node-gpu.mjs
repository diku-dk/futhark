// Make Dawn's WebGPU implementation available as navigator.gpu, as in a
// browser.  Loaded with 'node --import' by node-runner.sh.

import { createRequire } from 'node:module';
import { pathToFileURL } from 'node:url';

// The 'webgpu' package is an ES module, so importing it directly would ignore
// NODE_PATH.  We resolve it as CommonJS does instead.
const webgpu = createRequire(import.meta.url).resolve('webgpu');
const { create, globals } = await import(pathToFileURL(webgpu));

Object.assign(globalThis, globals);
Object.defineProperty(globalThis.navigator, 'gpu', { value: create([]) });
