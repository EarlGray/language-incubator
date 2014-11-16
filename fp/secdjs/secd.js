function Cons(hd, tl) {
    if (hd == undefined && tl == undefined) {
        this.nil = true;
    } else if (hd !== undefined && tl != undefined) {
        this.nil = false;
        this.hd = hd;
        this.tl = tl;
    } else {
        console.log('Error: Cons(' + hd + ', ' + tl + ')');
    }
    return this;
}

Cons.prototype.toString = function () {
    if (this.nil) return '()';
    return '(' + this.hd.toString() + ' : ' + this.tl.toString() + ')';
}

Cons.prototype.is_nil = function () { return this.nil; }
Cons.prototype.car = function () { return this.hd; }
Cons.prototype.cdr = function () { return this.tl; }

Cons.prototype.cadr = function () { return this.tl.hd; }
Cons.prototype.cddr = function () { return this.tl.tl; }

function arr2cons(arr) {
    var lst = new Cons();
    while (arr.length > 0)
        lst = new Cons(arr.pop(), lst);
    return lst;
}
function cons(hd, tl) { return new Cons(hd, tl); }
function nil() { return new Cons(); }

function Secd(ctrl) {
    this.stack = nil();
    this.env = nil(); // TODO
    this.ctrl = ctrl;
    this.dump = nil();
    return this;
}

Secd.prototype.debug = function (msg) {
    console.log(msg);
    /*
    console.log('S = ' + this.stack);
    console.log('E = ' + this.env);
    console.log('C = ' + this.ctrl);
    console.log('D = ' + this.dump);
    // */
}

Secd.prototype.pop_ctrl = function () {
    var hd = this.ctrl.car(); 
    this.ctrl = this.ctrl.cdr();
    return hd;
}

Secd.prototype.pop_stack = function () {
    var hd = this.stack.car();
    this.stack = this.stack.cdr();
    return hd;
}
Secd.prototype.push_stack = function (val) {
    this.stack = cons(val, this.stack);
}

Secd.prototype.pop_dump = function () {
    var hd = this.dump.car();
    this.dump = this.dump.cdr();
    return hd;
}
Secd.prototype.push_dump = function (val) {
    return (this.dump = cons(val, this.dump));
}

Secd.prototype.new_frame = function (args, argv) {
    return cons(args, argv);
}
Secd.prototype.lookup = function(sym) {
    var e = this.env;
    while (!e.is_nil()) {
        var frame = e.car();
        var args = frame.car();
        var argv = frame.cdr();
        while (!args.is_nil()) {
            if (args.car() == sym)
                return argv.car();
            args = args.cdr();
            argv = argv.cdr();
        }
        e = e.cdr();
    }
    return undefined;
}
Secd.prototype.print_env = function() {
    var e = this.env;
    while (!e.is_nil()) {
        var frame = e.car();
        console.log('Frame:');
        var args = frame.car();
        var argv = frame.cdr();
        while (!args.is_nil()) {
            console.log('   ' + args.car() + '\t -> ' + argv.car().toString());
            args = args.cdr();
            argv = argv.cdr();
        }
        e = e.cdr();
    }
}

Secd.prototype.cmdset = {
    'LDC': function (secd) {
        var val = secd.pop_ctrl();
        secd.debug('LDC ' + val);
        secd.push_stack(val);
    },
    'ADD': function (secd) {
        var a = secd.pop_stack();
        var b = secd.pop_stack();
        secd.debug('ADD ' + a + ' ' + b);
        secd.push_stack(a + b);
    },
    'SUB': function (secd) {
        var a = secd.pop_stack();
        var b = secd.pop_stack();
        secd.debug('SUB ' + a + ' ' + b);
        secd.push_stack(a - b);
    },
    'MUL': function(secd) {
        var a = secd.pop_stack();
        var b = secd.pop_stack(); 
        secd.debug('MUL ' + a + ' ' + b);
        secd.push_stack(a * b);
    },
    'EQ': function(secd) {
        var a = secd.pop_stack();
        var b = secd.pop_stack();
        secd.debug('EQ ' + a + ' ' + b);
        secd.push_stack(a == b ? 1 : 0);
    },
    'CONS': function (secd) {
        var hd = secd.pop_stack();
        var tl = secd.pop_stack();
        secd.debug('CONS ' + hd + '        ' + tl);
        secd.push_stack(cons(hd, tl));
    },
    'SEL': function(secd) {
        var thenb = secd.pop_ctrl();
        var elseb = secd.pop_ctrl();
        var cond = secd.pop_stack();

        secd.push_dump(secd.ctrl);
        if (cond) {
            secd.debug('SEL then');
            secd.ctrl = thenb;
        } else {
            secd.debug('SECD else');
            secd.ctrl = elseb;
        }
    },
    'JOIN': function(secd) {
        secd.debug('JOIN');
        secd.ctrl = secd.pop_dump();
    },
    'LDF': function(secd) {
        var fundef = secd.pop_ctrl();
        var clos = arr2cons([fundef, secd.env]);
        secd.debug('LDF (' + fundef.car() + ') ...');
        secd.push_stack(clos);
    },
    'AP': function(secd) {
        var clos = secd.pop_stack();
        var argv = secd.pop_stack();
        
        var fundef = clos.car();
        var cenv = clos.cdr();
        
        var args = fundef.car();
        var functrl = fundef.cadr();

        secd.debug('AP');
        // push current continuation
        secd.push_dump(secd.ctrl);
        secd.push_dump(secd.env);
        secd.push_dump(secd.stack);

        secd.stack = nil();
        secd.env = cons(secd.new_frame(args, argv), cenv);
        secd.ctrl = functrl;

        secd.print_env();
    },
    'LD': function (secd) {
        var sym = secd.pop_ctrl();
        secd.debug('LD ' + sym);
        var val = secd.lookup(sym);
        if (val == undefined) 
            console.log('LD: ' + sym + ' not found');
        secd.push_stack(val);
    },
    'RTN': function (secd) {
        var stack = secd.pop_dump();
        var env = secd.pop_dump();
        var ctrl = secd.pop_dump();

        var res = secd.pop_stack();
        secd.debug('RTN ' + res);
        
        secd.stack = cons(res, stack);
        secd.env = env;
        secd.ctrl = ctrl;
    },
    'DUM': function (secd) {
        secd.debug('DUM');
        secd.env = cons(nil(), secd.env);
    },
    'RAP': function (secd) {
        var clos = secd.pop_stack();
        var argv = secd.pop_stack();
        
        var fundef = clos.car();
        var cenv = clos.cdr();
        
        var args = fundef.car();
        var functrl = fundef.cadr();

        cenv.hd = secd.new_frame(args, argv);
        secd.debug('RAP');

        // push current continuation
        secd.push_dump(secd.ctrl);
        secd.push_dump(secd.env.cdr());
        secd.push_dump(secd.stack);

        secd.stack = nil();
        secd.env = cenv;
        secd.ctrl = functrl;
        
        secd.print_env();
    },
};

Secd.prototype.run = function () {
    while (true) {
        var op = this.pop_ctrl();
        if (op == 'STOP') {
            console.log('_______________________');
            console.log('Result is ' + this.pop_stack());
            break;
        }
        if (!op) {
            console.log('unexpected end of commands');
            break;
        }
        var opcode = this.cmdset[op];
        if (!opcode) {
            console.log('Error: no function for ' + op);
            break;
        }

        opcode(this);
    }
    console.log('\nSECD state:');
    console.log('stack = ' + this.stack);
    console.log('env = ' + this.env);
}

