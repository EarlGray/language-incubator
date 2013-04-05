#ifndef __BRAINFUCK_H__
#define __BRAINFUCK_H__

#define TAPE_SIZE		100000
//30720
#define OUT_BUF			1024

#define BF_NO_ERROR 		(0)
#define BF_ERR_INVALID_CMD	(-1)
#define BF_ERR_OUT_OF_MEM	(-2)
#define BF_ERR_NESTING		(-3)

#include <iostream>
using namespace std;

template <class T> class List;

class BFProgram
{
protected:
    struct Command;
    typedef List<Command> CommandList;

    Command *program;
    unsigned length;
	unsigned current;

    int compile(const char *prog, CommandList& commands);
    int optimize(CommandList& cmds);
    int compose(CommandList& cmds, Command* program[]);

public:
	BFProgram(const char *prog);
	virtual ~BFProgram();
	int interpret(istream &in, ostream &out);
};

#endif
