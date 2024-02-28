const wasmCode = Deno.readFileSync("./test.wasm");
const wasmModule = new WebAssembly.Module(wasmCode);

let memory = undefined;

function print_string(ptr, len) {
  let str = (new TextDecoder()).decode(new Uint8Array(memory.buffer, ptr, len));
  console.log(str);
}

let wasmInstance;
try {
  wasmInstance = new WebAssembly.Instance(wasmModule, {
    env: { print_string },
  });
} catch (error) {
  console.error(error);
}

memory = wasmInstance.exports.memory;

try {
  wasmInstance.exports.main();
  // TODO print results
  let ints = new Int32Array(memory.buffer);
  console.log(ints[ints.length - 2]);
} catch (error) {
  console.error(error);
}
