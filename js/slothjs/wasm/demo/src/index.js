import './style.css';

class Sljs {
    constructor(esprima, sljs) {
        this.esprima = esprima;
        this.sljs = sljs;
    }

    interpret(expr) {
        let jexpr = this.esprima.parseScript(expr);
        return this.sljs.interpret(jexpr);
    }
}

class Repl {
    static prompt = "sljs> ";

    constructor(el, sljs) {
        this.el = el;
        this.sljs = sljs;
    }

    static newElement(tag, attrs, ...children) {
        let el = document.createElement(tag);
        for (let k  in attrs) {
            el.setAttribute(k, attrs[k]);
        }
        if (children) {
            el.append(...children);
        }
        return el;
    }

    static newDetails(summary, ...contents) {
        let s = Repl.newElement('summary', {}, summary);
        return Repl.newElement('details', {}, s, ...contents);
    }

    startNewCell() {
        let inputspan = Repl.newElement('input', {class: 'input', size: 40});
        let promptspan = Repl.newElement('span', {class: 'prompt'}, Repl.prompt);
        let cmddiv = Repl.newElement('div', {class: 'command'}, promptspan, inputspan);

        this.cell = Repl.newElement('div', {'data-id': 1, class: 'cell'}, cmddiv)
        this.el.append(this.cell);

        inputspan.addEventListener('change', (event) => {
            this.onCellChange(event)
        });
        inputspan.focus();
    }
    
    onCellChange(event) {
        event.target.readOnly = true;
        event.target.disabled = true;
        let input = event.target.value;
        this.evalCell(input);

        this.startNewCell();
    }

    evalCell(input) {
        try {
            let output = this.sljs.interpret(input);
            let outdiv = Repl.newElement('div', {class: 'output'}, output);
            this.cell.append(outdiv);
        } catch (e) {
            console.error(e);
            let errfold = (e.stack ? Repl.newDetails(e, e.stack) : Repl.newElement('div', {}, e));
            errfold.classList.add('error', 'output');
            this.cell.append(errfold);
        }
    }
}

export let repl;

let welcome;
let appdiv;

const greetings = [
    "Yo, I've heard you like Javascript, so I've put a Javascript interpreter in WASM in your browser, so you can Javascript while you're Javascripting",
    "The world would be a better place with more... Javascript interpreters compiled to WASM!",
    "Imagine all the browsers can run a Javascript interpreter in WASM!",
    "\"A Javascript interpreter compiled to WASM\" sounded like a good idea at the time.",
    "What can be better than Javascript in your browser? Javascript interpreter in your browser's WASM!",
];

(function() {
    let hdr = Repl.newElement('h1', {id: 'hdr', style: 'margin-left: 1rem;'}, 'sljs')
    let i = Math.floor(Math.random() * greetings.length);
    let desc = Repl.newElement('div', {id: 'hello'}, greetings[i]);

    let source = Repl.newElement('a', {href: 'https://github.com/EarlGray/language-incubator/tree/main/js/slothjs'}, 'source');
    let docs = Repl.newElement('a', {href: 'https://earlgray.github.io/language-incubator/js/slothjs/index.html'}, 'documentation');
    let demo = Repl.newElement('a', {href: 'https://dmytrish.net/sljs'}, 'demo');
    let menu = Repl.newElement('div', {id: 'menu'}, source, docs, demo);

    welcome = Repl.newElement('div', {id: 'welcome', class: 'welcome'}, 'Loading...');
    appdiv = Repl.newElement('div', {id: 'app'}, welcome)
    document.body.append(menu, Repl.newElement('hr'),hdr, desc, Repl.newElement('hr'),  appdiv);
})();

Promise.all([
    import(/* webpackPrefetch: true */"esprima"),
    import(/* webpackPrefetch: true */"slothjs-wasm"),
]).then(({0: esprima, 1: sljs}) => {
    welcome.innerHTML = 'Welcome to sljs!'

    let interpreter = new Sljs(esprima, sljs);
    repl = new Repl(appdiv, interpreter);
    repl.startNewCell();
    repl.cell.querySelector('input').value = '2 + 2';
    repl.onCellChange({ target: repl.cell.querySelector('input')});
}).catch((e) => {
    console.error(e);
    let errfold = (e.stack ? Repl.newDetails(e, e.stack) : Repl.newElement('div', {}, e));
    errfold.classList.add('error');
    appdiv.append(errfold);
});
