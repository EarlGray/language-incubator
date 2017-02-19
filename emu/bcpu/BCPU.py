from __future__ import print_function


class BCPUError(Exception):
    pass


class BCPUSyntaxError(BCPUError):
    def __init__(self, reason, linenum=None):
        self.reason = reason
        self.linenum = linenum


class BCPUAssemblyError(BCPUError):
    def __init__(self, reason, linenum=None):
        self.reason = reason
        self.linenum = linenum


class BCPURuntimeError(BCPUError):
    pass


class BCPUAssembler:
    @staticmethod
    def parse(inp, debug=False):
        prog = {}
        linenum = 0

        block = BCPU.EMPTY_LABEL
        commands = []
        # seen_labels = set()

        for line in inp.readlines():
            linenum += 1
            label, op = BCPUAssembler.parse_line(line, linenum, debug)
            if label:
                commands.append(['JMP', label])
                prog[block] = commands

                block = label
                commands = []
            if op:
                commands.append(op)

        # do we need to stop automatically?
        if commands[-1][0] != 'HALT':
            commands.append(['HALT'])
        prog[block] = commands

        return prog

    @staticmethod
    def parse_line(s, linenum=None, debug=False):
        # get rid of comments
        s = s.split('#')[0]

        s = s.split()
        if not s:
            return (None, [])

        # get label
        label = None
        if s[0].endswith(':'):
            label = s[0][:-1]
            if not label.isalpha():
                raise BCPUSyntaxError(
                    'label must be alphanumeric, not `%s`' % label,
                    linenum)
            s = s[1:]
            if not s:
                return (label, [])

        op = []
        # get opcode
        if len(s[0]) == 3:
            if s[0].startswith('LD'):
                reg = BCPUAssembler.reg_at(s[0], 2, linenum)
                imm = BCPUAssembler.imm16bit(s[1], linenum)
                op = ['LD', reg, imm]
            elif s[0].startswith('RD'):
                reg = BCPUAssembler.reg_at(s[0], 2, linenum)
                loc = BCPUAssembler.imm16bit(s[1], linenum)
                op = ['RD', reg, loc]
            elif s[0].startswith('WT'):
                reg = BCPUAssembler.reg_at(s[0], 2, linenum)
                loc = BCPUAssembler.imm16bit(s[1], linenum)
                op = ['WT', reg, loc]
            elif s[0] in ['DIV', 'MOD', 'MUL']:
                op = [s[0]]
            elif s[0] in ['JOF', 'JUF', 'JMP']:
                dst = BCPUAssembler.read_label(s[1], linenum)
                op = [s[0], dst]
            elif s[0][0] == 'J' and s[0][2] == 'Z':
                reg = BCPUAssembler.reg_at(s[0], 1, linenum)
                dst = BCPUAssembler.read_label(s[1], linenum)
                op = ['JZ', reg, dst]
            else:
                raise BCPUAssemblyError(
                    'Unknown command: `%s`' % s[0], linenum)

        elif len(s[0]) == 4:
            if s[0].startswith('INC'):
                reg = BCPUAssembler.reg_at(s[0], 3, linenum)
                op = ['INC', reg]
            elif s[0].startswith('DEC'):
                reg = BCPUAssembler.reg_at(s[0], 3, linenum)
                op = ['DEC', reg]
            elif s[0].startswith('OUT'):
                reg = BCPUAssembler.reg_at(s[0], 3, linenum)
                op = ['OUT', reg]
            elif s[0] in ['HALT']:
                op = [s[0]]
            else:
                raise BCPUAssemblyError(
                    'Unknown command: `%s`' % s[0], linenum)
        else:
            raise BCPUAssemblyError(
                'Unknown command: `%s`' % s[0], linenum)

        if debug:
            print('[%3d] %s%s' % (linenum, label+':\t' if label else '\t', op))
        return (label, op)

    @staticmethod
    def reg_at(command, pos, linenum):
        reg = command[pos]
        if reg in BCPU.registers:
            return reg
        raise BCPUAssemblyError(
            'Invalid register `%s`' % reg, linenum)

    @staticmethod
    def imm16bit(arg, linenum):
        try:
            imm = int(arg)
            if not(0 <= imm and imm < 0x10000):
                raise BCPUAssemblyError(
                    'Value is not 16-bit: 0x%x' % imm, linenum)
            return imm
        except ValueError:
            raise BCPUSyntaxError(
                'Number expected instead of `%s`' % arg, linenum)

    @staticmethod
    def read_label(arg, linenum):
        if arg.isalpha():
            return arg
        raise BCPUAssemblyError(
            'Expected a label name instead of `%s`' % arg, linenum)


class BCPU:
    memlen = 0x10000
    registers = "ABCD"
    flags = ['OVERFLOW', 'UNDERFLOW']
    EMPTY_LABEL = ''

    def __init__(self, debug=False):
        """ Takes an external program to run, starting from `entry`
        `prog` is a dictionary from labels to basic blocks;
        A basic block is a list of opcodes with arguments.
        """
        self.debug = debug

        # assume memory is initialized with zeros
        self.mem = [0] * BCPU.memlen
        self.reg = {R: 0 for R in BCPU.registers}
        self.flag = {F: False for F in BCPU.flags}

    def run(self, prog, entry=''):
        self.exe_block = entry
        self.exe_offset = 0

        while True:
            op = prog[self.exe_block][self.exe_offset]
            if op[0] == 'HALT':
                break
            op_fun = getattr(self, op[0])
            op_arg = op[1:]
            cont = op_fun(*op_arg)
            if cont:
                self.exe_block = cont
                self.exe_offset = 0
            else:
                self.exe_offset += 1

    def log(self, msg):
        self.debug and print(msg)

    def readmem(self, loc):
        if loc < 0 or loc + 1 >= BCPU.memlen:
            raise BCPURuntimeError('Invalid memory address: *0x%4x', loc)
        # are reads always aligned?
        # assume this is a little-endian architecture?
        b1 = self.mem[loc]
        b2 = self.mem[loc + 1]
        return b1 + b2 * 256

    def writemem(self, loc, val):
        if loc < 0 or loc + 1 >= BCPU.memlen:
            raise BCPURuntimeError('Invalid memory address: *0x%4x' % loc)
        if val < 0 or val >= 0x10000:
            raise BCPURuntimeError('Value is not 16-bit: 0x%x' % val)
        self.mem[loc] = val % 0x100
        self.mem[loc+1] = int(val / 0x100)

    def LD(self, reg, val):
        self.reg[reg] = val

    def RD(self, reg, loc):
        val = self.readmem(loc)
        self.reg[reg] = val

    def WT(self, reg, loc):
        val = self.reg[reg]
        self.writemem(loc, val)

    def INC(self, reg):
        val = self.reg[reg]
        if val < 0xFFFF:
            self.reg[reg] += 1
        else:
            self.reg[reg] = 0
            self.flag['OVERFLOW'] = True

    def DEC(self, reg):
        val = self.reg[reg]
        if val > 0:
            self.reg[reg] -= 1
        else:
            self.reg[reg] = 0xFFFF
            self.flag['UNDERFLOW'] = True

    def DIV(self):
        self.reg['C'] = self.reg['A'] / self.reg['B']

    def MOD(self):
        self.reg['C'] = self.reg['A'] % self.reg['B']

    def MUL(self):
        self.reg['C'] = self.reg['A'] * self.reg['B']

    def JZ(self, reg, label):
        return label if self.reg[reg] == 0 else None

    def JOF(self, label):
        if self.flag['OVERFLOW']:
            return label

    def JUF(self, label):
        if self.flag['UNDERFLOW']:
            return label

    def JMP(self, label):
        return label

    def OUT(self, reg):
        print(self.reg[reg])

    def HALT(self):
        raise BCPURuntimeError('HALT executed')


if __name__ == '__main__':
    import sys
    if len(sys.argv) < 2:
        inp = sys.stdin
    else:
        inp = open(sys.argv[1], 'r')

    try:
        prog = BCPUAssembler.parse(inp)
        # print('### ' + str(prog))

        bcpu = BCPU()
        bcpu.run(prog)
    except BCPUSyntaxError as e:
        sys.stderr.write(
            'SYNTAX ERROR (line %s): %s\n' % (e.linenum or '??', e.reason))
    except BCPUAssemblyError as e:
        sys.stderr.write(
            'ASSEMBLY ERROR (line %s): %s\n' % (e.linenum or '??', e.reason))
    except BCPUError as e:
        sys.stderr.write('RUNTIME ERROR: %s\n' % e)
