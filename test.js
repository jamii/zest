const wasmCode = Deno.readFileSync(Deno.args[0] ?? "./test.wasm");
const wasmModule = new WebAssembly.Module(wasmCode);

let memory = undefined;

let wasmInstance;
try {
  wasmInstance = new WebAssembly.Instance(wasmModule, {
    env: {
      print: function (ptr, len) {
        let str = new Uint8Array(memory.buffer, ptr, len);
        Deno.writeAllSync(Deno.stdout, str);
      },
    },
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
