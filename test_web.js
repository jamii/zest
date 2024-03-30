let memory = undefined;
let bytes = undefined;

WebAssembly.instantiateStreaming(fetch("./test.wasm"), {
  env: {},
}).then(
  (results) => {
    const wasmInstance = results.instance;
    memory = wasmInstance.exports.memory;
    const result = wasmInstance.exports.main();
    console.log(result);
  },
);
