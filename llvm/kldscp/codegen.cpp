#include <iostream>
#include <map>

#include <llvm/Core.h>
#include <llvm/LLVMContext.h>

#include "ast.h"

typedef std::map<std::string, Value *> varmap;

struct codegen_t {
    Module *module;
    IRBuilder<> builder;
    varmap namedValues;

    codegen_t() {
        this->builder = getGlobalContext();
    }
};

Value *errorValue(const char *errmsg) {
    std::cout << errmsg << std::endl;
    return nullptr;
}

Value *codegen(ast_t *ast) {
    switch (ast->type) {
      case AST_NUM:
        return ConstantFP::get(getGlobalContext(), APFloat(ast->as_num));
      case AST_VAR:
        varmap::iterator var = namedValues.find(ast->as_var);
        if (var == std::end) return errorValue("Can't find variable");
        return var;
      case AST_BINOP:
        Value *lval = codegen(ast->as_binop.lhs);
        Value *rval = codegen(ast->as_binop.rhs);
        if (!lval || !rval) return errorValue("codegen(binop): a branch failed");
    }
}
