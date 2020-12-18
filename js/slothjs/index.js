const fs = require('fs');
const esprima = require('esprima');

let filename = process.argv[2] || '/dev/stdin';

let contents = fs.readFileSync(filename, 'utf8');
let ast = esprima.parse(contents);
console.log(JSON.stringify(ast, null, 2));
