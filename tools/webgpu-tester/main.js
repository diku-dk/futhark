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
		return;
	}

	const templateRun = testInfo.runs[0];

	// Create input buffers.
	let inputBuffers = [];
	for (let i = 0; i < templateRun.input.length; i++) {
		if (templateRun.inputTypes[i] != "i32") {
			console.error("Only i32 supported atm.");
			return;
		}
		const inputElemSize = 4;
		
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

	let outputBuffer = device.createBuffer({
		size: maxLength * 4, // TODO: elem size
		usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_SRC, 
	});
	let stagingBuffer = device.createBuffer({
		size: maxLength * 4, // TODO: elem size
		usage: GPUBufferUsage.MAP_READ | GPUBufferUsage.COPY_DST, 
	});

	let scalarBuffer = device.createBuffer({
		size: 8,
		usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
	});

	let bglEntries = [
		{ binding: 0, visibility: GPUShaderStage.COMPUTE,
		  buffer: { type: "uniform" } }
	];
	let bgEntries = [
		{ binding: 0, resource: { buffer: scalarBuffer } }
	];
	for (let i = 0; i < inputBuffers.length; i++) {
		bglEntries.push({
			binding: i + 1, visibility: GPUShaderStage.COMPUTE,
			buffer: { type: "storage" }
		});
		bgEntries.push({
			binding: i + 1, resource: { buffer: inputBuffers[i] }
		});
	}
	bglEntries.push({
		binding: inputBuffers.length + 1,
		visibility: GPUShaderStage.COMPUTE,
		buffer: { type: "storage" } }
	);
	bgEntries.push({
		binding: inputBuffers.length + 1,
		resource: { buffer: outputBuffer }
	});
	const bgl = device.createBindGroupLayout({
		entries: bglEntries
	});
	const bg = device.createBindGroup({ layout: bgl, entries: bgEntries });

	let bgls = [];
	let bgs = [];
	for (let i = 0; i <= kernelInfo.group; i++) {
		if (i == kernelInfo.group) {
			bgls.push(bgl);
			bgs.push(bg);
		}
		else {
			const emptyBgl = device.createBindGroupLayout({ entries: [] });
			bgls.push(emptyBgl);
			bgs.push(device.createBindGroup({ layout: emptyBgl, entries: [] }));
		}
	}

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
		layout: device.createPipelineLayout({ bindGroupLayouts: bgls }),
		compute: {
			module: shaderModule, entryPoint: kernelInfo.name,
			constants: overrideConsts,
		}
	});

	for (let ri = 0; ri < testInfo.runs.length; ri++) {
		const run = testInfo.runs[ri];
		const length = run.input[0].length;
		let hScalars = new Int32Array(2);
		hScalars[0] = length;
		hScalars[1] = 0;
		device.queue.writeBuffer(scalarBuffer, 0, hScalars, 0);

		for (let i = 0; i < run.input.length; i++) {
			const input = run.input[i];
			let hInput = new Int32Array(input.length);
			for (let j = 0; j < input.length; j++) {
				hInput[j] = input[j];
			}
			device.queue.writeBuffer(inputBuffers[i], 0, hInput, 0);
		}

		const commandEncoder = device.createCommandEncoder();
		const passEncoder = commandEncoder.beginComputePass();
		passEncoder.setPipeline(pipeline);

		for (let i = 0; i <= kernelInfo.group; i++) {
			passEncoder.setBindGroup(i, bgs[i]);
		}

		passEncoder.dispatchWorkgroups(Math.ceil(length / block_size));
		passEncoder.end();
		commandEncoder.copyBufferToBuffer(
			outputBuffer, 0, stagingBuffer, 0, length * 4);
		device.queue.submit([commandEncoder.finish()]);

		await stagingBuffer.mapAsync(GPUMapMode.READ, 0, length * 4);
		const stagingMapped = stagingBuffer.getMappedRange(0, length * 4);
		const data = new Int32Array(stagingMapped.slice());
		stagingBuffer.unmap();

		let errors = [];
		for (let i = 0; i < length; i++) {
			if (data[i] != run.expected[0][i]) {
				errors.push({i: i, expect: run.expected[0][i], got: data[i]});
			}
		}

		if (errors.length > 0) {
			let msg = `Test for entry ${testInfo.entry}, run ${ri}: FAIL:\n`;
			for (const err of errors) {
				msg += `  [${err.i}] expected ${err.expect} got ${err.got}\n`;
			}
			console.error(msg);
		}
		else {
			console.log(`Test for entry ${testInfo.entry}, run ${ri}: PASS\n`);
		}
	}
}

function getTestName() {
	const urlParams = new URLSearchParams(window.location.search);
	return urlParams.get('test');
}

async function evalTestInfo(testName) {
	const testPath = `./${testName}.js`;
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

	const testName = getTestName();

	await evalTestInfo(testName);

	const shaderModule = device.createShaderModule({
		code: shader,
	});

	const shaderInfo = await shaderModule.getCompilationInfo();
	console.log("Shader compilation info:", shaderInfo);

	for (const test of window.tests) {
		await runTest(device, shaderModule, test);
	}
}

init();
