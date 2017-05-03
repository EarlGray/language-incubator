#include <vector>
#include <string>
#include <fstream>
#include <iostream>
#include <streambuf>
#include <cstdint>


typedef uint32_t addr_t;

const int MEMSIZE = 30000;

const char *syntax = "+-<>.,[]";

enum Op {
    Inc,
    Dec,
    Left,
    Right,
    Print,
    Read,
    Begin,
    End
};

typedef std::vector<Op> Ops;

Ops compile(const std::string &prog) {
    Ops ops;
    for (auto c : prog) {
        switch (c) {
            case '+': ops.push_back(Op::Inc); break;
            case '-': ops.push_back(Op::Dec); break;
            case '<': ops.push_back(Op::Left); break;
            case '>': ops.push_back(Op::Right); break;
            case '.': ops.push_back(Op::Print); break;
            case ',': ops.push_back(Op::Read); break;
            case '[': ops.push_back(Op::Begin); break;
            case ']': ops.push_back(Op::End); break;
        }
    }
    return ops;
}

addr_t find_jump(const Ops& ops, addr_t cp, Op to) {
    int nest = 0;
    addr_t i = cp;

    off_t step;
    Op from;
    switch (to) {
    case Op::Begin:
        from = Op::End;
        step = -1;
        break;
    case Op::End:
        from = Op::Begin;
        step = 1;
        break;
    default:
        throw "find_jump: Op::Begin or Op::End expected";
    }
//    std::cerr << "looking for " << syntax[to] << std::endl;
    while (true) {
        i += step;

        Op op = ops.at(i);
//        std::cerr << "i=" << i << std::endl;

        if (op == from) {
            ++nest;
        } else if (op == to) {
            if (nest > 0) {
                --nest;
            } else if (nest == 0) {
                return i;
            } else {
                throw "find_jump: malformed loop";
            }
        }
    }
}

void interpret(const Ops& ops, std::istream &input, std::ostream &output) {
    uint32_t cp = 0;
    addr_t dp = 0;

    uint8_t mem[MEMSIZE] = { 0 };

    while (true) {
        char c;
        Op op = ops.at(cp);
//        std::cerr << "exe[" << cp << "]: " << syntax[op] << std::endl;

        switch (op) {
        case Op::Inc:
            mem[dp] += 1;
            break;
        case Op::Dec:
            mem[dp] -= 1;
            break;
        case Op::Left:
            dp -= 1;
            break;
        case Op::Right:
            dp += 1;
            break;
        case Op::Print:
            output.put((char)mem[dp]);
            break;
        case Op::Read:
            input >> c;
            mem[dp] = (uint8_t)c;
            break;
        case Op::Begin:
            if (mem[dp] == 0) {
                cp = find_jump(ops, cp, Op::End);
                continue;
            }
            break;
        case Op::End:
            if (mem[dp] != 0) {
                cp = find_jump(ops, cp, Op::Begin);
                continue;
            }
            break;
        }
        ++cp;
        if (cp >= ops.size())
            break;
    }
}

void run(const std::string& source) {
    const Ops ops = compile(source); 

    interpret(ops, std::cin, std::cout);
}

int main(int argc, char **argv) {
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " <source.bf>" << std::endl;
        return 1;
    }
    const char* progfile = argv[1];    

    std::ifstream f(progfile);

    std::string contents = std::string(
        std::istreambuf_iterator<char>(f),
        std::istreambuf_iterator<char>()
    );

    run(contents);
}
