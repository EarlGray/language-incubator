#ifndef __CODEGN_KLDSCP_H__
#define __CODEGN_KLDSCP_H__

#ifdef __cpluplus
extern "C" {
#endif


void test_interp();
void test_codegen();

#ifdef __cpluplus
}
#endif

#ifdef __KLD_OPT
#include "ast.h"
#include <llvm/Function.h>
#include <llvm/Type.h>

class Optimizer {
public:
    virtual void run(llvm::Function *) = 0;
    virtual void operator() (llvm::Function *);
    virtual ~Optimizer() {}
};

void Optimizer::operator()(llvm::Function *func) {
    this->run(func);
}

llvm::Value *codegen(ast_t *ast);
llvm::Function *codegenFunDef(ast_t *ast, Optimizer *optimize);

#endif

#endif //__CODEGN_KLDSCP_H__
