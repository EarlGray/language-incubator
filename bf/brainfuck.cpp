#include "brainfuck.h"
#include <cstdlib>
#include <cstring>

#include "std.h"

#define BF_OP_NEXT	'>'
#define BF_OP_PREV	'<'
#define BF_OP_INC	'+'
#define BF_OP_DEC	'-'
#define BF_OP_IF	'['
#define BF_OP_ENDIF	']'
#define BF_OP_IN	','
#define BF_OP_OUT	'.'

#define BF_AOP_LOAD 'l'         // load [this] = p
#define BF_AOP_MOVE 'm'         // move this +=  p
#define BF_AOP_ADD  'a'         // add  [this] += p
#define BF_AOP_SUM  's'         // sum  [this] += [this + p]

/// -----------------------------------------------------------------

class Tape 
{
protected:
	char tape[TAPE_SIZE];
	int pointer;

public:
	Tape();
	int move(int count = 1);
	int add(int x = 1);
	char get_cell();
	void set_cell(char c);
	char& operator[] (int i);
};

Tape::Tape():
	pointer(0)
{
	memset(tape, 0, TAPE_SIZE);
}

inline char& Tape::operator[](int i)
{
	return tape[i];
}

inline int Tape::move(int count) {
    if ((pointer - count < 0) && (pointer + count >= TAPE_SIZE))
		return BF_ERR_OUT_OF_MEM;
    pointer += count;
    return BF_NO_ERROR;
}

inline int Tape::add(int x) {
	tape[pointer] += x;
	return BF_NO_ERROR;
}

inline char Tape::get_cell() {
	return tape[pointer];
}

inline void Tape::set_cell(char c) {
	tape[pointer] = c;
}


/// -----------------------------------------------------------------

struct BFProgram::Command  {
    char op;
    int count; // address of match for [, ] or repetition count for other

    Command(char op, unsigned count):
            op(op), count(count)
    {}

    Command():
            op('+'),
            count(0)
    {}

    void print(int num)
    {
        cout << num << ") " << op << " : " << count << endl;
    }
};


BFProgram::BFProgram(const char *prog):
	current(0)
{
    CommandList commands;       // commands
    compile(prog, commands);
    optimize(commands);
    compose(commands, &program);
}

BFProgram::~BFProgram()
{
	delete[] program;
}

int BFProgram::compile(const char *prog, CommandList& commands)
{
    typedef Stack<unsigned> StackUnsigned;

    StackUnsigned cycles;       // addresses of cycles

    current = 0;                // program counter for program (Commands)
    unsigned pc = 0;            // program counter for prog (chars)

    while (prog[pc]) {
        switch (prog[pc]) 
        {
        case BF_OP_IF: {
                commands.add(Command(BF_OP_IF, 0));
                cycles.push(current);
                ++current;
            } break;
        case BF_OP_ENDIF: {
                unsigned match = cycles.pop();
                commands.add(Command(BF_OP_ENDIF, match));
                *(commands.item(match)) = Command(BF_OP_IF, current);
                ++current;
            } break;
        case BF_OP_NEXT:
        case BF_OP_PREV:
        case BF_OP_INC:
        case BF_OP_DEC:
        case BF_OP_IN:
        case BF_OP_OUT: {
                unsigned start_pc = pc;
                while (prog[pc] == prog[pc+1]) ++pc;
                commands.add(Command(prog[pc], pc - start_pc + 1));
                ++current;
            } break;
        }
        ++pc;
    }
}

int BFProgram::compose(CommandList& cmds, Command* program[])
{
    length = cmds.length();
    Command *p = new Command[length];
    CommandList::Node* cmd = cmds.getFirst();

    for (unsigned i = 0; i < length; ++i) {
        p[i] = cmd->data;
        //p[i].print(i);
        cmd = cmd->next;
    }
    *program = p;
    return BF_NO_ERROR;
}

int BFProgram::optimize(CommandList& cmds) 
{
    // >< to j()
    // +- to a()
    // remove duplicates and zero additions
    CommandList::Node* cmd = cmds.getFirst();       // current command
    CommandList::Node* pcmd = NULL, *ppcmd = NULL;   // previous and pre-previous

    while (cmd != NULL) {
        switch (cmd->data.op) {
        case BF_OP_DEC:
            cmd->data.count = -(cmd->data.count);
            // fall through
        case BF_OP_INC:
            cmd->data.op = BF_AOP_ADD;
            /*
            if (pcmd == NULL) break;

            // merge consequent + and - to add(n)
            if (pcmd->data.op == cmd->data.op) {
                pcmd->data.count += cmd->data.count;
                pcmd->next = cmd->next;
                delete cmd;
                cmd = pcmd;
                pcmd = ppcmd;
                ppcmd = NULL;
            }
            if (pcmd == NULL) break;

            // if add(0), eliminate operation
            if (cmd->data.count == 0) {
                pcmd->next = cmd->next;
                delete cmd;
                cmd = pcmd;
                pcmd = ppcmd;
                ppcmd = NULL;
            }*/
            break;

        case BF_OP_PREV:
            cmd->data.count = -(cmd->data.count);
        case BF_OP_NEXT: 
            cmd->data.op = BF_AOP_MOVE;
            break;
        }
        ppcmd = pcmd;
        pcmd = cmd;
        cmd = cmd->next;
    }
         
    return 0;
}

int BFProgram::interpret(istream &in, ostream &out)
{	
	Tape tape;
    current = 0;
    
    while (current < length) 
	{
		switch (program[current].op)
		{
		case BF_AOP_MOVE:
			tape.move(program[current].count);
			break;

		case BF_AOP_ADD:
			tape.add(program[current].count);
			break;

        /*case BF_AOP_ZERO:
            tape.set_cell(0);
            break;
        */
		case BF_OP_IN:
			{
				char c;
                for (int i = 0; i < program[current].count; ++i)
                    in.read(&c, 1);
				tape.set_cell(c);
			}
			break;

		case BF_OP_OUT:
            for (int i = 0; i < program[current].count; ++i)
    			out << tape.get_cell();
			break;

		case BF_OP_IF:
			if (! tape.get_cell()) 
				current = program[current].count;
			break;

		case BF_OP_ENDIF:
			if ( tape.get_cell())
				current = program[current].count;
			break;
		}
		++current;
	}	
	return BF_NO_ERROR;
}

