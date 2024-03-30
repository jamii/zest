const wasmCode = Deno.readFileSync("./test.wasm");
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
  console.log(result);
} catch (error) {
  console.error(error);
}
