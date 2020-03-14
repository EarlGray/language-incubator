const fs = require('fs');
const esprima = require('esprima');

let filename = process.argv[2];
if (!filename) {
    console.error('A file name is expected');
    process.exit();
}

let contents = fs.readFileSync(filename, 'utf8');
let ast = esprima.parse(contents);
console.log(JSON.stringify(ast, null, 2));
