#include <iostream>
#include <map>
#include <vector>
#include <stdarg.h>

#include <llvm/Function.h>
#include <llvm/Module.h>
#include <llvm/Type.h>
#include <llvm/LLVMContext.h>
#include <llvm/Support/IRBuilder.h>

#include "ast.h"

using namespace llvm;

typedef std::map<std::string, Value * > varmap;

static Module *theModule;
static IRBuilder<> theBuilder(getGlobalContext());
static varmap theVarStore;

Value *errorVal(const char *msg, ...) {
    va_list va;
    va_start(va, msg);
    vfprintf(stderr, msg, va);
    va_end(va);
    return nullptr;
}

Value *codegen(ast_t *ast) {
    switch (ast->type) {
      case AST_NUM:
        return ConstantFP::get(getGlobalContext(), APFloat(ast->as_num));
      case AST_VAR: {
        Value *val = theVarStore[ast->as_var];
        return val ? val : errorVal("Unknown variable: %s\n", ast->as_var);
      }
      case AST_BINOP: {
        Value *lval = codegen(ast->as_binop.lhs);
        Value *rval = codegen(ast->as_binop.rhs);
        if (!rval || !lval) 
            return errorVal("codegen(binop): a branch failed\n");

        switch (ast->as_binop.op) {
          case '+': return theBuilder.CreateFAdd(lval, rval, "addtmp");
          case '-': return theBuilder.CreateFAdd(lval, rval, "subtmp");
          case '*': return theBuilder.CreateFMul(lval, rval, "multmp");
          case '<':
            lval = theBuilder.CreateFCmpULT(lval, rval, "cmptmp");
            return theBuilder.CreateUIToFP(lval, 
                                Type::getDoubleTy(getGlobalContext()),
                                "booltmp");
          default: return errorVal("codegen: unknown binop %c\n", ast->as_binop.op);
        }
      }
      case AST_IF:
        break;
      case AST_CALL: {
        const char *funname = ast->as_funcall.name;
        Function *callee = theModule->getFunction(funname);
        if (!callee)
            return errorVal("codegen(funcall): unknown function %s\n", funname);

        size_t n_found = list_length(ast->as_funcall.args);
        size_t n_expected = callee->arg_size();
        if (n_found != n_expected) 
            return errorVal("codegen(funcall): expected %d arguments, found %d\n", n_expected, n_found);

        std::vector<Value *> argvals;
        list_node_t *arg = list_head(ast->as_funcall.args);
        while (arg) {
            Value *argval = codegen((ast_t*)list_data(arg));
            if (!argval) return errorVal("codegen(funcall): codegen(arg) failed\n");
            argvals.push_back(argval);
            arg = list_next(arg);
        }
        return theBuilder.CreateCall(callee, argvals, "calltmp");
      }

      case AST_FUNDEF: {
        // create its prototype
        std::string funname(ast->as_fundef.name);
        std::vector<Type *> doubles(list_length(ast->as_fundef.args),
                                    Type::getDoubleTy(getGlobalContext()));
        FunctionType *functype = FunctionType::get(Type::getDoubleTy(getGlobalContext()), doubles, false);
        Function *func = Function::Create(functype, Function::ExternalLinkage, funname, theModule);
        } break;
    }
    return errorVal("no val\n");
}
