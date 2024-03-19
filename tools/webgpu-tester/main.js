function typeSize(type) {
	if (type == 'i32') { return 4; }
	if (type == 'u32') { return 4; }
	if (type == 'i64') { return 8; }
	if (type == 'u64') { return 8; }
	if (type == 'f32') { return 4; }
	throw "unsupported type";
}

function toTypedArray(array, type) {
	if (type == 'i32') { return new Int32Array(array); }
	if (type == 'u32') { return new Uint32Array(array); }
	if (type == 'i64') {
		const dest = new Int32Array(array.length * 2);
		for (let i = 0; i < array.length; i++) {
			dest[i*2] = Number(BigInt.asIntN(32,
				array[i] & 0xffffffffn));
			dest[i*2+1] = Number(BigInt.asIntN(32,
				(array[i] >> 32n) & 0xffffffffn));
		}
		return dest;
	}
	if (type == 'u64') {
		const dest = new Uint32Array(array.length * 2);
		for (let i = 0; i < array.length; i++) {
			dest[i*2] = Number(BigInt.asUintN(32,
				array[i] & 0xffffffffn));
			dest[i*2+1] = Number(BigInt.asUintN(32,
				(array[i] >> 32n) & 0xffffffffn));
		}
		return dest;
	}
	if (type == 'f32') { return new Float32Array(array); }
	throw "unsupported type";
}

function arrayBufferToTypedArray(buffer, type) {
	if (type == 'i32') { return new Int32Array(buffer); }
	if (type == 'u32') { return new Uint32Array(buffer); }
	if (type == 'i64') { return new Int32Array(buffer); }
	if (type == 'u64') { return new Uint32Array(buffer); }
	if (type == 'f32') { return new Float32Array(buffer); }
	throw "unsupported type";
}

function compareExpected(val, expected, type) {
	if (type == 'f32') {
		return val == expected || (isNaN(val) && isNaN(expected));
	}
	return val == expected;
}

async function runTest(device, shaderModule, testInfo) {
	// Find kernel corresponding to entry.
	let kernelInfo = undefined;
	for (const k of window.kernels) {
		if (k.name.includes(testInfo.entry)) {
			kernelInfo = k;
			break;
		}
	}
	if (kernelInfo === undefined) {
		console.error("Could not find kernel info for", testInfo);
		return {0: "❌ failed to find kernel info"};
	}

	const templateRun = testInfo.runs[0];

	// Create input buffers.
	let inputBuffers = [];
	for (let i = 0; i < templateRun.input.length; i++) {
		const inputElemSize = typeSize(templateRun.inputTypes[i]);
		
		// Find maximum required size for buffer.
		let maxLength = 0;
		for (const run of testInfo.runs) { 
			if (run.input[i].length > maxLength) {
				maxLength = run.input[i].length;
			}
		}

		const buffer = device.createBuffer({
			size: maxLength * inputElemSize,
			usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST,
		});
		inputBuffers.push(buffer);
	}


	let maxLength = 0;
	for (const run of testInfo.runs) { 
		if (run.expected[0].length > maxLength) {
			maxLength = run.expected[0].length;
		}
	}

	// TODO: This is not always true.
	const outputType = templateRun.inputTypes[0];
	const outputElemSize = typeSize(outputType);

	let outputBuffer = device.createBuffer({
		size: maxLength * outputElemSize,
		usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_SRC, 
	});
	let stagingBuffer = device.createBuffer({
		size: maxLength * outputElemSize,
		usage: GPUBufferUsage.MAP_READ | GPUBufferUsage.COPY_DST, 
	});

	let scalarBuffer = device.createBuffer({
		size: 8,
		usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
	});

	let bglEntries = [
		{ binding: kernelInfo.bindSlots[0], visibility: GPUShaderStage.COMPUTE,
		  buffer: { type: "uniform" } }
	];
	let bgEntries = [
		{ binding: kernelInfo.bindSlots[0], resource: { buffer: scalarBuffer } }
	];
	for (let i = 0; i < inputBuffers.length; i++) {
		bglEntries.push({
			binding: kernelInfo.bindSlots[i + 1],
			visibility: GPUShaderStage.COMPUTE,
			buffer: { type: "storage" }
		});
		bgEntries.push({
			binding: kernelInfo.bindSlots[i + 1],
			resource: { buffer: inputBuffers[i] }
		});
	}
	bglEntries.push({
		binding: kernelInfo.bindSlots[inputBuffers.length + 1],
		visibility: GPUShaderStage.COMPUTE,
		buffer: { type: "storage" } }
	);
	bgEntries.push({
		binding: kernelInfo.bindSlots[inputBuffers.length + 1],
		resource: { buffer: outputBuffer }
	});
	const bgl = device.createBindGroupLayout({
		entries: bglEntries
	});
	const bg = device.createBindGroup({ layout: bgl, entries: bgEntries });

	const block_size = 256;
	let overrideConsts = {};
	for (const override of kernelInfo.overrides) {
		if (override.includes('lockstep_width')) {
			overrideConsts[override] = 1;
		}
		if (override.includes('block_size')
			|| override.includes('block_sizze')) {
			overrideConsts[override] = block_size;
		}
	}

	const pipeline = device.createComputePipeline({
		layout: device.createPipelineLayout({ bindGroupLayouts: [bgl] }),
		compute: {
			module: shaderModule, entryPoint: kernelInfo.name,
			constants: overrideConsts,
		}
	});

	let results = {};

	for (let ri = 0; ri < testInfo.runs.length; ri++) {
		const run = testInfo.runs[ri];
		const length = run.input[0].length;
		let hScalars = new Int32Array(2);
		hScalars[0] = length;
		hScalars[1] = 0;
		device.queue.writeBuffer(scalarBuffer, 0, hScalars, 0);

		for (let i = 0; i < run.input.length; i++) {
			const input = run.input[i];
			const inputType = run.inputTypes[i];

			let hInput = toTypedArray(input, inputType);
			device.queue.writeBuffer(inputBuffers[i], 0, hInput, 0);
		}

		const commandEncoder = device.createCommandEncoder();
		const passEncoder = commandEncoder.beginComputePass();
		passEncoder.setPipeline(pipeline);

		passEncoder.setBindGroup(0, bg);

		passEncoder.dispatchWorkgroups(Math.ceil(length / block_size));
		passEncoder.end();
		commandEncoder.copyBufferToBuffer(
			outputBuffer, 0, stagingBuffer, 0, length * outputElemSize);
		device.queue.submit([commandEncoder.finish()]);

		await stagingBuffer.mapAsync(GPUMapMode.READ, 0, length * outputElemSize);
		const stagingMapped = stagingBuffer.getMappedRange(0, length * outputElemSize);
		const data = arrayBufferToTypedArray(stagingMapped.slice(), outputType);
		stagingBuffer.unmap();

		const expected = toTypedArray(run.expected[0], outputType);

		let errors = [];
		for (let i = 0; i < expected.length; i++) {
			if (!compareExpected(data[i], expected[i], outputType)) {
				errors.push({i: i, expect: expected[i], got: data[i]});
			}
		}

		if (errors.length > 0) {
			let msg = `Test for entry ${testInfo.entry}, run ${ri}: FAIL:\n`;
			for (const err of errors) {
				msg += `  [${err.i}] expected ${err.expect} got ${err.got}\n`;
			}
			console.error(msg);

			results[ri] = "❌";
		}
		else {
			console.log(`Test for entry ${testInfo.entry}, run ${ri}: PASS\n`);
			results[ri] = "✅";
		}
	}

	return results;
}

async function getTestNames() {
	const urlParams = new URLSearchParams(window.location.search);

	if (urlParams.get('tests-file')) {
		const response = await fetch(urlParams.get('tests-file'));
		const tests = await response.json();
		return tests;
	}

	return [urlParams.get('test')];
}

async function evalTestInfo(testName) {
	const testPath = `./tests/${testName}.js`;
	console.log(`Test info from: ${testPath}`);

	const response = await fetch(testPath);
	const testInfo = await response.text();
	// Executes the JS in testInfo in global scope instead of in the local
	// scope.
	(1, eval)(testInfo);
}

async function init() {
	if (!navigator.gpu) {
		throw Error("WebGPU not supported.");
	}

	const adapter = await navigator.gpu.requestAdapter();
	if (!adapter) {
		throw Error("Couldn't request WebGPU adapter.");
	}

	const info = await adapter.requestAdapterInfo();
	console.log("Adapter info: ", info);

	const device = await adapter.requestDevice();
	console.log("Acquired device: ", device);

	const testNames = await getTestNames();

	var results = {};

	for (const testName of testNames) {
		try {
			await evalTestInfo(testName);
		} catch (e) {
			results[testName] = { compiled: false, res: {
				"[❌ could not load testInfo:]": {0: e} } };
			continue;
		}

		const shaderModule = device.createShaderModule({
			code: shader,
		});

		const shaderInfo = await shaderModule.getCompilationInfo();
		console.log(`${testName}: Shader compilation info:`, shaderInfo);

		if (shaderInfo.messages.length > 0) {
			results[testName] = { compiled: false, res: {} };
		}
		else {
			try {
				var testResults = {};
				for (const test of window.tests) {
					const r = await runTest(device, shaderModule, test);
					testResults[test.entry] = r;
				}
				results[testName] = { compiled: true, res: testResults };
			} catch (e) {
				results[testName] = { compiled: true, res: {
					"[❌ error]": {0: e} } };
				continue;
			}
		}
	}

	renderTestResults(results);
}

function renderTestResults(results) {
	const container = document.getElementById("results");
	for (const [testName, testResults] of Object.entries(results)) {
		const h = document.createElement("h3");
		h.innerHTML = testName;
		container.appendChild(h);

		const c = document.createElement("span");
		c.classList = "compiled";
		c.innerHTML = "Compiled: " + (testResults.compiled ? "✅" : "❌");
		container.appendChild(c);

		for (const [entry, entryRes] of Object.entries(testResults.res)) {
			const eh = document.createElement("h4");
			eh.innerHTML = entry;
			container.appendChild(eh);

			for (const [run, runRes] of Object.entries(entryRes)) {
				const r = document.createElement("span");
				r.classList = "run";
				r.innerHTML = run + ": " + runRes;
				container.appendChild(r);
			}
		}
	}
}

init();
