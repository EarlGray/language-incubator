#include <cassert>
#include <cstdarg>
#include <cstdio>

//#include <iostream>
#include <map>
#include <vector>

#include <llvm/Module.h>
#include <llvm/Function.h>
#include <llvm/Type.h>
#include <llvm/LLVMContext.h>
#include <llvm/Support/IRBuilder.h>
#include <llvm/Analysis/Verifier.h>

#include "ast.h"

using namespace llvm;

typedef std::map<std::string, Value * > varmap;

static Module *theModule;
static IRBuilder<> theBuilder(getGlobalContext());
static varmap theVarStore;

void *errorVal(const char *msg, ...) {
    va_list va;
    va_start(va, msg);
    vfprintf(stderr, msg, va);
    va_end(va);
    return nullptr;
}


Value *codegen(ast_t *ast);

Value *codegenBinop(ast_t *ast) {
    assert(ast->type == AST_BINOP);
    Value *lval = codegen(ast->as_binop.lhs);
    Value *rval = codegen(ast->as_binop.rhs);
    if (!rval || !lval)
        return (Value *)errorVal("codegen(binop): a branch failed\n");

    switch (ast->as_binop.op) {
      case '+': return theBuilder.CreateFAdd(lval, rval, "addtmp");
      case '-': return theBuilder.CreateFAdd(lval, rval, "subtmp");
      case '*': return theBuilder.CreateFMul(lval, rval, "multmp");
      case '<':
        lval = theBuilder.CreateFCmpULT(lval, rval, "cmptmp");
        return theBuilder.CreateUIToFP(lval,
                            Type::getDoubleTy(getGlobalContext()),
                            "booltmp");
      default: return (Value *)errorVal("codegen: unknown binop %c\n", ast->as_binop.op);
    }
}

Value *codegenVar(ast_t *ast) {
    assert(ast->type == AST_VAR);
    Value *val = theVarStore[ast->as_var];
    return val ? val : (Value *)errorVal("Unknown variable: %s\n", ast->as_var);
}

Value *codegenIf(ast_t *ast) {
    assert(ast->type == AST_IF);
    return (Value *)errorVal("codegenIf: TODO\n");
}

Value *codegenFunCall(ast_t *ast) {
    assert(ast->type == AST_CALL);
    const char *funname = ast->as_funcall.name;
    Function *callee = theModule->getFunction(funname);
    if (!callee)
        return (Value *)errorVal("codegen(funcall): unknown function %s\n", funname);

    size_t n_found = list_length(ast->as_funcall.args);
    size_t n_expected = callee->arg_size();
    if (n_found != n_expected)
        return (Value *)errorVal("codegen(funcall): expected %d arguments, found %d\n", n_expected, n_found);

    std::vector<Value *> argvals;
    list_node_t *arg = list_head(ast->as_funcall.args);
    while (arg) {
        Value *argval = codegen((ast_t*)list_data(arg));
        if (!argval)
            return (Value *)errorVal("codegen(funcall): codegen(arg) failed\n");
        argvals.push_back(argval);
        arg = list_next(arg);
    }
    return theBuilder.CreateCall(callee, argvals, "calltmp");
}

Function *codegenProto(ast_t *ast) {
    assert(ast->type == AST_FUNDEF);

    size_t n_args = ast->as_fundef.args ? list_length(ast->as_fundef.args) : 0;
    std::string funname(ast->as_fundef.name);
    std::vector<Type *> doubles(n_args, Type::getDoubleTy(getGlobalContext()));

    FunctionType *functype = FunctionType::get(Type::getDoubleTy(getGlobalContext()), doubles, false);
    Function *func = Function::Create(functype, Function::ExternalLinkage, funname, theModule);

    // WTF? Why is it not checked beforehand?
    if (func->getName() != funname) {
        func->eraseFromParent();
        func = theModule->getFunction(funname);

        if (! func->empty())
            return (Function *)errorVal("codegenProto: redefinition\n");

        if (func->arg_size() != n_args)
            return (Function *)errorVal("codegenProto: redefinition with different argument count\n");
    }

    list_node_t *arg = list_head(ast->as_fundef.args);
    for (Function::arg_iterator ai = func->arg_begin();
         ai != func->arg_end(); ++ai, arg = list_next(arg))
    {
        std::string argname((const char *)list_data(arg));
        ai->setName(argname);
        theVarStore[argname] = ai;
    }
    return func;
}

Function *codegenFunDef(ast_t *ast) {
    assert(ast->type == AST_FUNDEF);
    theVarStore.clear();

    Function *func = codegenProto(ast);
    if (!func)
        return (Function *)errorVal("codegenFunDef: codegenProto() failed\n");

    BasicBlock *bb = BasicBlock::Create(getGlobalContext(), "entry", func);
    theBuilder.SetInsertPoint(bb);

    ast_t *astbody = ast->as_fundef.body;
    if (!astbody)
        return func; // prototype

    Value *bodyval = codegen(ast->as_fundef.body);
    if (! bodyval) {
        func->eraseFromParent();
        return (Function *)errorVal("codegenFunDef: codegen(body) failed\n");
    }

    theBuilder.CreateRet(bodyval);
    verifyFunction(*func);

    return func;
}

inline Value *codegenNum(ast_t *ast) {
    assert(ast->type == AST_NUM);
    return ConstantFP::get(getGlobalContext(), APFloat(ast->as_num));
}

Value *codegen(ast_t *ast) {
    assert(ast);
    switch (ast->type) {
      case AST_NUM:     return codegenNum(ast);
      case AST_VAR:     return codegenVar(ast);
      case AST_BINOP:   return codegenBinop(ast);
      case AST_IF:      return codegenIf(ast);
      case AST_CALL:    return codegenFunCall(ast);
      //case AST_FUNDEF:  return codegenFunDef(ast);
      default: break;
    }
    return (Value *)errorVal("codegen(): unknown ast->type %d\n", ast->type);
}

void test_codegen() {
    parser_t p;
    parseinit(&p, new_lexer(getchar));

    while(1) {
        printf("#IR# ");
        ast_t *ast = parse_toplevel(&p);

        if (! ast) {
            if (parse_eof(&p)) break;
            if (parsed_semicolon(&p)) continue;

            fprintf(stderr, "==========================\n PARSER ERROR\n\n");
        }

        switch (ast->type) {
          case AST_FUNDEF: {
            Function *irfunc = codegenFunDef(ast);
            if (irfunc) {
                irfunc->dump();
            }
            else fprintf(stderr, "test_codegen: codegenFunDef failed\n");
          } break;

          case AST_BINOP: case AST_VAR: case AST_IF:
          case AST_CALL: case AST_NUM: {
            Value *irval = codegen(ast);
            if (irval) {
                irval->dump();
            }
            else
                fprintf(stderr, "test_codegen: codegen() failed\n");
          } break;
        }

        free_ast(ast);
    }
}
