#include <iostream>
#include <fstream>
#include <vector>
#include <cstring>
#include "brainfuck.h"
using namespace std;

void usage()
{
	cout << "Brainfuck interpreter by Dmytrish\nThis is a beerware\n";
	cout << "Usage: \n\tbf -e <program> | -f <file>\n\n";
}

int open_bf(const char *fname, char **prog)
{
	ifstream fin(fname);
	if (!fin)
	{
		cerr << "Can`t open file " << fname << "\n\n";
		return 1;
	}

	char c;
	vector<char> p;
	while (fin)
	{
		fin.read(&c, 1);
		p.push_back(c);
	}

	*prog = new char[p.size() + 1];
	for (int i = 0; i < p.size(); ++i)
		(*prog)[i] = p.at(i);
	(*prog)[p.size()] = 0;

	return 0;
}

int main(int argc, char *argv[])
{
	char *prog;
	if (argc < 3)
	{
		usage();
		return -1;
	}

	if (!strcmp(argv[1], "-e"))
	{
		prog = new char[strlen(argv[2]) + 1];
		strcpy(prog, argv[2]);
	}
	else if (!strcmp(argv[1], "-f"))
	{
		int hr = 0;
		if ((hr = open_bf(argv[2], &prog)) != 0) 
			return hr;
	}
	else 
	{
		cerr << "Unknown argument: " << argv[1] << "\n\n";
		return -2;
	}

	BFProgram *bf = new BFProgram(prog);
	delete[] prog;

	int hr = 0;
	hr = bf->interpret(cin, cout);
	if (hr)
	{
		cerr << "Error executing program: " << hr << "\n";
	}

	delete bf;
	return 0;
}
