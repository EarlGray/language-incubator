#ifndef __KLDSCP_PARSER__H_
#define __KLDSCP_PARSER__H_

#include <istream>
#include <string>
#include <vector>

/*
 *  Lexer
 */

typedef int Token;

class Lexer {
public:
    enum Token {
        END = -1, 
        DEF = -2,
        EXT = -3,
        ID = -4,
        BINOP = -5,
        NUM = -6,
        IF = -7,
        THEN = -8,
        ELSE = -9,

        START = 0xffff
    };

private:
    int last;
    int prev;
    std::istream *inp;

    int tok;
    int prevtok;

    std::string strtok;
    double numtok;
    char chrtok;

    int nextchar();
    Lexer& nexttok(int tok);

public:
    Lexer(std::istream *inp): 
        inp(inp), last(' '), tok(Token::START) 
    {}

    int token() const;

    std::string strval() const;
    double numval() const;
    char chrval() const;

    Lexer & next();

    int precedence(char op);
};

int Lexer::token() const {
    return this->last;
}

std::string Lexer::strval() const {
    return this->strtok;
}

double Lexer::numval() const {
    return this->numtok;
}

char Lexer::chrval() const {
    return this->chrtok;
}

/*
 *  Abstract syntax tree
 */
#ifndef LLVM_MODULE_H
class llvm::Value;
#endif

struct ASTNode {
    virtual void print(int indent) = 0;
    virtual llvm::Value * codegen() = 0;
};

struct ASTNumber : public ASTNode {
    double val;

    ASTNumber(double val): 
        val(val)
    {}
    void print(int indent);
}; 

struct ASTIdentifier : public ASTNode {
    std::string name;

    ASTIdentifier(const std::string &name):
        name(name)
    {}

    void print(int indent);
};

struct ASTBinop : public ASTNode {
    char op;
    int prec;
    ASTNode *lhs, *rhs;

    ASTBinop(char op, ASTNode *left, ASTNode *right, int prec):
        op(op), lhs(left), rhs(right), prec(prec)
    {}

    ~ASTBinop();

    void print(int indent);
};

struct ASTIfThenElse : public ASTNode {
    ASTNode *condb;
    ASTNode *thenb, *elseb;

    ASTIfThenElse(ASTNode *condb, ASTNode *thenb, ASTNode *elseb):
        condb(condb), thenb(thenb), elseb(elseb)
    {}
    ~ASTIfThenElse(); 

    void print(int indent);
};

struct ASTFunCall : public ASTNode {
    std::vector<ASTNode *> args;
    std::string name;

    ASTFunCall(const std::string name, const std::vector<ASTNode *> args):
        name(name), args(args)
    {}
    ~ASTFunCall();

    void print(int indent);
};

struct ASTFunDef : public ASTNode {
    std::string name;
    std::vector< std::string > args;
    ASTNode *body;

    ASTFunDef(const std::string name, const std::vector< std::string > args, ASTNode *body):
        name(name), args(args), body(body)
    {}
    ~ASTFunDef();

    void print(int indent);
};

/*
 * Parser
 */

class Parser {
private:
    Lexer *lex;

    int tok;
    int prevtok;

    int nexttok();

    ASTNode *expr();
    ASTNumber *number();
    ASTNode *parenExpr();
    ASTNode *identExpr();
    ASTNode *cond();
    ASTNode *binop(ASTNode *lhs);
    ASTNode *primary();
    ASTFunDef *proto();
    ASTFunDef *funcDef();
    ASTFunDef *externDef();

    ASTNode *error(const char *msg, ...);
public:
    Parser(Lexer *lex): 
        lex(lex)
    {}

    ASTNode *toplevel();

    bool eof();
    bool parsed_semicolon();

    ~Parser();
};

#endif //__KLDSCP_PARSER__H_
