const wasmCode = Deno.readFileSync(Deno.args[0] ?? "./test.wasm");
const wasmModule = new WebAssembly.Module(wasmCode);

let memory = undefined;

let wasmInstance;
try {
  wasmInstance = new WebAssembly.Instance(wasmModule, {
    env: {},
  });
} catch (error) {
  console.error(error);
}

memory = wasmInstance.exports.memory;

try {
  const result = wasmInstance.exports.main();
  // Use result.toString to print Int64 without a trailing 'n'
  console.log(result?.toString());
} catch (error) {
  console.error(error);
}
