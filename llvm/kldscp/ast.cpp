#include <cctype>
#include <cstdlib>
#include <map>

#include "ast.h"

#define DEBUG  (0)

#define assertf(cond, ...) do { \
    if (!cond) { fprintf(stderr, __VA_ARGS__); return NULL; } \
    } while (0)

/*
 * Binary operations
 */

#define DUMMY_PRECEDENCE   (-1)
#define PAREN_PRECEDENCE   100

typedef std::map< char, int > precedmap_t;

const precedmap_t initial_precedence = {
    { '+', 10},
    { '-', 20},
    { '*', 40},
    { '/', 50},
    { '<', 5 },
};


/*
 *  Lexer
 */

const std::map< std::string, Lexer::Token > keywords = {
    { "if",     Lexer::IF },
    { "then",   Lexer::THEN },
    { "else",   Lexer::ELSE },
};

int Lexer::nextchar() {
    this->prev = this->last;
    return (this->last = this->inp->get());
}

Lexer& Lexer::nexttok(int tok) {
    this->prevtok = this->tok;
    this->tok = tok;
    return *this;
}

int Lexer::precedence(char op) {
    auto itprec = initial_precedence.find(op);
    if (itprec == std::map::end)
        return DUMMY_PRECEDENCE;
    return *itprec;
}

Lexer& Lexer::next() {
    while (isspace(this->last))
        nextchar();

    // parse identifiers|keywords
    if (isalpha(this->last)) {
        this->strtok.clear();

        do this->strtok += this->last;
        while (isalnum(nextchar()));

        auto it = keywords.find(this->strtok);
        if (it == std::map::end)
            return nexttok(*it);

        return nexttok(Token::ID);
    }

    // parse numbers
    if (isdigit(this->last) || (this->last == '.')) {
        this->strtok.clear();

        do {
            this->strtok += this->last;
            nextchar();
        } while (isdigit(this->last) || this->last == '.');
        
        if (this->strtok == ".")
            return nexttok('.');

        size_t read = 0;
        this->numtok = std::stod(this->strtok, &read);
    
        return nexttok(Token::NUM);
    }

    // comments
    if (this->last == '#') {
        do nextchar();
        while (this->last != EOF && this->last != '\n');

        if (this->last != EOF)
            return this->next();  // yes, recursion
    }

    if (this->last == EOF)
        return nexttok(Token::EOF);

    // maybe binary operation
    if (precedence(this->last) != DUMMY_PRECEDENCE) {
        this->chrtok = this->last;
        nextchar();
        return nexttok(Token::BINOP);
    }

    // a symbol
    int c = this->last;
    nextchar();
    return c;
}


/*
 *  Parser
 */

int Parser::nexttok() {
    this->prevtok = this->tok;
    this->tok = this->lex->next().token();
#if DEBUG
    printf("next_token: ");
    switch (this->tok) {
      case Lexer::Token::EOF: printf("TOK_EOF\n"); return;
      case TOK_NUM: printf("TOK_NUM(%f)\n", p->lex->numtoken); return;
      case TOK_ID: printf("TOK_ID(%s)\n", p->lex->strtoken); return;
      case TOK_BINOP: printf("TOK_BINOP(%c)\n", p->lex->op); return;
      case TOK_DEF: printf("TOK_DEF\n"); return;
      case TOK_IF: printf("TOK_IF\n"); return;
      case TOK_EXT: printf("TOK_EXT\n"); return;
      default: printf("TOKEN<%c>\n", p->token); 
    }
#endif
    return this->tok;
}


ASTNode * Parser::error(const char *msg, ...) {
    va_list va;
    va_start(va, msg);
    vfprintf(stderr, msg, va);
    va_end(va);
    nexttok();
    return nullptr;
}


ASTNumber * Parser::number() {
    assertf(this->tok == Lexer::NUM, "number expected\n");
    ASTNumber *num = new ASTNumber(this->lex->numval());
    nexttok();
    return num;
}


ASTNode * Parser::parenExpr() {
    assertf(this->tok == '(', "parse_paren_expr: expected '('\n");
    nexttok(p);

    ASTNode *expr = parseExpr();
    if (!expr) return nullptr;

    ASTBinop *binop = dynamic_cast<ASTBinop *>(expr);
    if (binop)
        expr->as_binop.preced = PAREN_PRECEDENCE;

    assertf(this->tok == ')', "parse_paren_expr: expected ')'\n");
    nexttok();
    return expr;
}

ASTNode *Parser::identExpr() {
    assertf(this->tok == Lexer::ID, "parse_id: TOK_ID expected\n");

    std::string identifier(lex->strval());
    nexttok();

    // it is a variable
    if (this->tok != '(') 
        return new ASTIdentifier(identifier);

    // it is a function call
    std::vector< ASTNode * > args;

    nexttok();
    if (this->tok == ')') 
        nexttok();
    else {
        while (true) {
            ASTNode *expr = expr();
            if (!expr) 
                return error("funcall arguments: failed to read expression\n");

            args.push_back(expr);

            if (this->tok == ',') {
                nexttok();
                continue;
            }

            if (this->tok == ')') {
                nexttok();
                break;
            }

            return error("funcall argmuents: a comma inspected\n");
        }
    }

    return new ASTFunCall(identifier, args);
}

ASTNode *Parser::cond() {
    assertf(this->tok == Lexer::IF, "parse_if: 'if'  expected\n");

    nexttok();
    ASTNode *condb = expr();
    assertf(condb, "parse_if: parse_expr(condition) failed\n");

    assertf(this->tok, Lexer::THEN, "parse_if: 'then' expected\n");

    nexttok();
    ASTNode *thenb = expr();
    assertf(thenb, "parse_if: parse_expr(thenb) failed\n");

    assertf(this->tok, Lexer::ELSE, "parse_if: 'else' expected\n");

    nexttok();
    ASTNode *elseb = expr();
    assertf(elseb, "parse_if: parse_expr(elseb) failed\n");

    return new ASTIfThenElse(cond, thenb, elseb);
}

ASTNode * Parser::binop(ASTNode *lhs) {
    int op1 = this->lex->chrval();
    int p1 = this->lex->precedence(op1);

    nexttok();
    ASTNode *rhs = expr();
    assertf(rhs, "parse_binop: parse_expr() failed\n");

    ASTBinop *binrhs = dynamic_cast<ASTBinop *>(rhs);
    if (! binrhs)
        return new ASTBinop(op1, lhs, binrhs, p1);

    int op2 = binrhs->op;
    int p2 = binrhs->prec;

    //printf("parse_binop(): '%c'[%d], '%c'[%d]\n", op1, p1, op2, p2);
    if (p1 < p2)
        return new ASTBinop(op1, lhs, expr, p1);

    // rebalance
    return new ASTBinop(op2,
            new ASTBinop(op1, lhs, rhs->lhs, p1),
            rhs->rhs, p2);
}

ASTNode *primary() {
    switch (this->tok) {
      case Lexer::ID: 
        return identExpr();
      case Lexer::NUM: 
        return number();
      case Lexer::IF:
        return cond();
      case '(' : 
        return parenExpr();
      default: 
        return error("parse_primary: unknown token '%1$c' (%1$d)\n", this->tok);
    }
}

ASTNode *expr() {
    ASTNode *e = primary();
    assertf(e, "parse_expr: parse_primary() failed\n");

    if (this->tok == Lexer::BINOP)
        return binop(e);

    return e;
}

ASTNode * Parser::proto() {
    assertf(this->tok == Lexer::ID, "parse_proto: expected function name\n");
    std::string fname(this->lex->strval());

    nexttok();
    assertf(this->tok == '(', "parse_proto: expected '('\n");

    std::vector<std::string> args;

    nexttok();
    if (this->tok != ')') {
        while (1) {
            assertf(this->tok == Lexer::ID, "parse_proto: an identifier expected\n");
            args.push_back(this->lex->strval());

            nexttok();
            if (this->tok == ')')
                break;

            if (this->tok != ',')
                return error("parse_ext: a comma expected\n");
            nexttok();
        }
    }

    nexttok();
    return new ASTFunDef(fname, args, nullptr);
}

ASTFunDef *Parser::funcDef() {
    assertf(this->tok == Lexer::DEF, "parse_def: 'def' expected\n");
    next_token(p);

    ast_t *fundef = parse_proto(p);
    assertf(fundef, "parse_def: parse_proto() failed\n");

    ast_t *body = parse_expr(p);
    assertf(body, "parse_def: body expected\n");

    fundef->as_fundef.body = body;
    return fundef;
}

ASTFunDef *Parser::externDef() {
    assertf(this->tok == Lexer::EXT, "parse_ext: TOK_EXT expected\n");

    nexttok();
    return proto();
}

ASTNode *Parser::toplevel() {
    switch (this->tok) {
      case Lexer::START: 
        nexttok(); 
        return toplevel();
      case Lexer::EOF:
        return nullptr;
      case Lexer::EXT: 
        return externDef();
      case Lexer::DEF: 
        return funcDef();
      case ';':
        nexttok();
        return nullptr;
      default: 
        return expr();
    }
}


/*
 *  Dumping AST
 */

/*
inline static void print_indent(int indent) {
    int i;
    for (i = 0; i < indent; ++i) printf("  ");
}

void print_ast(int indent, ast_t *ast) {
    switch (ast->type) {
      case AST_NUM:
        print_indent(indent);
        printf("NUM(%f)\n", ast->as_num);
        break;
      case AST_VAR:
        print_indent(indent);
        printf("VAR(%s)\n", ast->as_var);
        break;
      case AST_BINOP:
        print_indent(indent); printf("BINOP(%c)\n", ast->as_binop.op);
        print_ast(indent + 1, ast->as_binop.lhs);
        print_ast(indent + 1, ast->as_binop.rhs);
        break;
      case AST_IF:
        print_indent(indent); printf("IF:\n");
        print_ast(indent + 1, ast->as_if.cond);
        print_indent(indent); printf("THEN:\n");
        print_ast(indent + 1, ast->as_if.thenb);
        print_indent(indent); printf("ELSE:\n");
        print_ast(indent + 1, ast->as_if.elseb);
        break;
      case AST_CALL:
        print_indent(indent); printf("CALL(%s)\n", ast->as_funcall.name);
        if (ast->as_funcall.args) {
            list_node_t *arg = list_head(ast->as_funcall.args);
            while (arg) {
                print_indent(indent); printf("ARG:\n");
                print_ast(indent + 1, (ast_t *)list_data(arg));
                arg = list_next(arg);
            }
        }
        break;
      case AST_FUNDEF:
        print_indent(indent);
        printf("DEF %s(", ast->as_fundef.name);
        // arguments
        if (ast->as_fundef.args) {
            list_node_t *arg = list_head(ast->as_fundef.args);
            while (arg) {
                printf("%s", (char *)list_data(arg));
                arg = list_next(arg);
                if (!arg) break;
                printf(", ");
            }
        }
        printf(")\n");

        // body
        ast_t* body = ast->as_fundef.body;
        if (body)
            print_ast(indent + 1, body);
        else {
            print_indent(indent + 1);
            printf("<extern>\n");
        }

        break;
      default:
        printf("PRINT ERROR: unknown ast->type %d\n", ast->type);
    }
}

*/
