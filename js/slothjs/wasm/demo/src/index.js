(function() {
    // TODO: use lodash templates instead
    let h1 = document.createElement('h1');
    h1.append('Welcome to sljs!');

    let hr = document.createElement('hr'); 
    let appdiv = document.createElement('div');
    appdiv.id = "app";
    appdiv.style = "white-space: pre; font-family: monospace;";

    document.body.append(h1, hr, appdiv);
})();

let appdiv = document.querySelector('#app');

Promise.all([
    import(/* webpackPrefetch: true */"esprima"),
    import(/* webpackPrefetch: true */"slothjs-wasm"),
]).then(({0: esprima, 1: sljs}) => {
    const expr = '2 + 2';
    let jexpr = esprima.parseScript(expr);
    let result = sljs.interpret(JSON.stringify(jexpr));
    appdiv.append(`${expr} = ${result}`);
    console.log('slothjs-wasm OK: ' + result);
}).catch((e) => {
    console.error('Loading slothjs-wasm failed: ' + e);
});
