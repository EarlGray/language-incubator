#include "codegen.h"

#include <cassert>
#include <cstdarg>
#include <cstdio>

//#include <iostream>
#include <map>
#include <vector>

#include <llvm/Module.h>
#include <llvm/LLVMContext.h>
#include <llvm/Support/IRBuilder.h>

#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/PassManager.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Analysis/AliasAnalysis.h>
#include <llvm/Analysis/InstructionSimplify.h>
#include <llvm/Target/TargetData.h>
#include <llvm/Transforms/Scalar.h>

#include "ast.h"

using namespace llvm;

typedef std::map<std::string, Value * > varmap;

struct codegen_t {
    Module *module;
    IRBuilder<> ir_builder;
    FunctionPassManager *fpm;
    varmap variables;

    codegen_t(): 
        ir_builder(getGlobalContext()), fpm(nullptr) 
    {}
};

void *errorVal(const char *msg, ...) {
    va_list va;
    va_start(va, msg);
    vfprintf(stderr, msg, va);
    va_end(va);
    return nullptr;
}


Value *codegen(codegen_t &cg, ast_t *ast);

Value *codegenBinop(codegen_t &cg, ast_t *ast) {
    assert(ast->type == AST_BINOP);
    IRBuilder<> &IR = cg.ir_builder;
    Value *lval = codegen(cg, ast->as_binop.lhs);
    Value *rval = codegen(cg, ast->as_binop.rhs);
    if (!rval || !lval)
        return (Value *)errorVal("codegen(binop): a branch failed\n");

    switch (ast->as_binop.op) {
      case '+': return IR.CreateFAdd(lval, rval, "addtmp");
      case '-': return IR.CreateFSub(lval, rval, "subtmp");
      case '*': return IR.CreateFMul(lval, rval, "multmp");
      case '/': return IR.CreateFDiv(lval, rval, "divtmp");
      case '<':
        lval = IR.CreateFCmpULT(lval, rval, "cmptmp");
        return IR.CreateUIToFP(lval,
                            Type::getDoubleTy(getGlobalContext()),
                            "booltmp");
      default: return (Value *)errorVal("codegen: unknown binop %c\n", ast->as_binop.op);
    }
}

Value *codegenVar(codegen_t &cg, ast_t *ast) {
    assert(ast->type == AST_VAR);
    Value *val = cg.variables[ast->as_var];
    return val ? val : (Value *)errorVal("Unknown variable: %s\n", ast->as_var);
}

Value *codegenIf(codegen_t &cg, ast_t *ast) {
    assert(ast->type == AST_IF);
    IRBuilder<> &IR = cg.ir_builder;
    Value *condval = codegen(cg, ast->as_if.cond);
    if (!condval)
        return (Value *)errorVal("codegenIf: codegen(cond) failed\n");

    condval = IR.CreateFCmpONE(condval,
            ConstantFP::get(getGlobalContext(), APFloat(0.0)),
            "ifcond");

    Function *func = IR.GetInsertBlock()->getParent();
    if (!func)
        return (Value*)errorVal("codegenIf: getParent() -> NULL\n");

    BasicBlock *bbThen = BasicBlock::Create(getGlobalContext(), "then", func);
    // these block are not inserted into 'func' now:
    BasicBlock *bbElse = BasicBlock::Create(getGlobalContext(), "else");
    BasicBlock *bbEnd =  BasicBlock::Create(getGlobalContext(), "ifcont");

    IR.CreateCondBr(condval, bbThen, bbElse);

    // insert 'then' branch
    IR.SetInsertPoint(bbThen);
    Value *thenval = codegen(cg, ast->as_if.thenb);
    if (!thenval)
        return (Value *)errorVal("codegenIf: codegen(then) failed\n");

    IR.CreateBr(bbEnd);
    bbThen = IR.GetInsertBlock(); // update the block

    // insert 'else' branch
    func->getBasicBlockList().push_back(bbElse);
    IR.SetInsertPoint(bbElse);
    Value *elseval = codegen(cg, ast->as_if.elseb);
    if (!elseval)
        return (Value *)errorVal("codegenIf: codegen(else) failed\n");

    IR.CreateBr(bbEnd);
    bbElse = IR.GetInsertBlock();

    // end block:
    func->getBasicBlockList().push_back(bbEnd);
    IR.SetInsertPoint(bbEnd);

    PHINode *phi = IR.CreatePHI(Type::getDoubleTy(getGlobalContext()), 2, "iftmp");
    phi->addIncoming(thenval, bbThen);
    phi->addIncoming(elseval, bbElse);

    return phi;
}

Value *codegenFunCall(codegen_t &cg, ast_t *ast) {
    assert(ast->type == AST_CALL);
    const char *funname = ast->as_funcall.name;
    Function *callee = cg.module->getFunction(funname);
    if (!callee)
        return (Value *)errorVal("codegen(funcall): unknown function %s\n", funname);

    size_t n_found = list_length(ast->as_funcall.args);
    size_t n_expected = callee->arg_size();
    if (n_found != n_expected)
        return (Value *)errorVal("codegen(funcall): expected %d arguments, found %d\n", n_expected, n_found);

    std::vector<Value *> argvals;
    list_node_t *arg = list_head(ast->as_funcall.args);
    while (arg) {
        Value *argval = codegen(cg, (ast_t*)list_data(arg));
        if (!argval)
            return (Value *)errorVal("codegen(funcall): codegen(arg) failed\n");
        argvals.push_back(argval);
        arg = list_next(arg);
    }
    return cg.ir_builder.CreateCall(callee, argvals, "calltmp");
}

Function *codegenProto(codegen_t &cg, ast_t *ast) {
    assert(ast->type == AST_FUNDEF);

    size_t n_args = ast->as_fundef.args ? list_length(ast->as_fundef.args) : 0;
    std::string funname(ast->as_fundef.name);
    std::vector<Type *> doubles(n_args, Type::getDoubleTy(getGlobalContext()));

    FunctionType *functype = FunctionType::get(Type::getDoubleTy(getGlobalContext()), doubles, false);
    Function *func = Function::Create(functype, Function::ExternalLinkage, funname, cg.module);

    // WTF? Why is it not checked beforehand?
    if (func->getName() != funname) {
        func->eraseFromParent();
        func = cg.module->getFunction(funname);

        if (! func->empty())
            return (Function *)errorVal("codegenProto: redefinition\n");

        if (func->arg_size() != n_args)
            return (Function *)errorVal("codegenProto: redefinition with different argument count\n");
    }

    list_node_t *arg = ast->as_fundef.args ? list_head(ast->as_fundef.args) : nullptr;
    for (Function::arg_iterator ai = func->arg_begin();
         ai != func->arg_end(); ++ai, arg = list_next(arg))
    {
        std::string argname((const char *)list_data(arg));
        ai->setName(argname);
        cg.variables[argname] = ai;
    }
    return func;
}

Function *codegenFunDef(codegen_t &cg, ast_t *ast) {
    assert(ast->type == AST_FUNDEF);
    cg.variables.clear();

    Function *func = codegenProto(cg, ast);
    if (!func)
        return (Function *)errorVal("codegenFunDef: codegenProto() failed\n");

    BasicBlock *bb = BasicBlock::Create(getGlobalContext(), "entry", func);
    cg.ir_builder.SetInsertPoint(bb);

    ast_t *astbody = ast->as_fundef.body;
    if (!astbody)
        return func; // prototype

    Value *bodyval = codegen(cg, ast->as_fundef.body);
    if (! bodyval) {
        func->eraseFromParent();
        return (Function *)errorVal("codegenFunDef: codegen(body) failed\n");
    }

    cg.ir_builder.CreateRet(bodyval);
    verifyFunction(*func);

    if (cg.fpm) 
        cg.fpm->run(*func);

    return func;
}

inline Value *codegenNum(codegen_t &cg, ast_t *ast) {
    assert(ast->type == AST_NUM);
    return ConstantFP::get(getGlobalContext(), APFloat(ast->as_num));
}

Value *codegen(codegen_t &cg, ast_t *ast) {
    assert(ast);
    switch (ast->type) {
      case AST_NUM:     return codegenNum(cg, ast);
      case AST_VAR:     return codegenVar(cg, ast);
      case AST_BINOP:   return codegenBinop(cg, ast);
      case AST_IF:      return codegenIf(cg, ast);
      case AST_CALL:    return codegenFunCall(cg, ast);
      //case AST_FUNDEF:  return codegenFunDef(ast);
      default: break;
    }
    return (Value *)errorVal("codegen(): unknown ast->type %d\n", ast->type);
}

extern "C" {

int lexnext(lexer_t *l) {
    return getchar();
}

void test_codegen(bool interactive) {
    codegen_t cg;
    LLVMContext &llvm_ctx = getGlobalContext();
    cg.module = new Module("kld", llvm_ctx);

    parser_t p;
    parseinit(&p, new_lexer(lexnext));

    if (interactive) printf("#IR# ");

    while(1) {
        ast_t *ast = parse_toplevel(&p);

        if (! ast) {
            if (parse_eof(&p)) break;
            if (parsed_semicolon(&p)) continue;

            fprintf(stderr, "==========================\n PARSER ERROR\n\n");
        }

        switch (ast->type) {
          case AST_FUNDEF: {
            Function *irfunc = codegenFunDef(cg, ast);
            if (irfunc) {
                if (interactive) 
                    irfunc->dump();
            }
            else fprintf(stderr, "test_codegen: codegenFunDef failed\n");
          } break;

          case AST_BINOP: case AST_VAR: case AST_IF:
          case AST_CALL: case AST_NUM: {
            Value *irval = codegen(cg, ast);
            if (irval) {
                irval->dump();
            }
            else
                fprintf(stderr, "test_codegen: codegen() failed\n");
          } break;
        }

        free_ast(ast);

        if (interactive) printf("#IR# ");
    }

    if (interactive) fprintf(stderr, "=======================================\n");
    cg.module->dump();
    if (interactive) {
        fprintf(stderr, "=======================================\n");
        fprintf(stderr, "Bye.\n");
    }
}

typedef double (*doublefunc_t)();

void test_interp() {
    parser_t p;
    parseinit(&p, new_lexer(lexnext));

    codegen_t cg;
    LLVMContext &llvm_ctx = getGlobalContext();
    cg.module = new Module("kld", llvm_ctx);

    InitializeNativeTarget();

    std::string errstr = "Error!";
    ExecutionEngine *theEngine = EngineBuilder(cg.module).setErrorStr(&errstr).create();
    if (!theEngine) {
        fprintf(stderr, "Failed to create ExecutionEngine: %s\n", errstr.c_str());
        return;
    }
    
    FunctionPassManager funcpass(cg.module);
    funcpass.add(new TargetData(*theEngine->getTargetData()));

    funcpass.add(createBasicAliasAnalysisPass());
    funcpass.add(createInstructionCombiningPass());
    funcpass.add(createReassociatePass());
    funcpass.add(createGVNPass());
    funcpass.add(createCFGSimplificationPass());

    funcpass.doInitialization();
    cg.fpm = &funcpass;

    printf("#KLD# ");
    while (1) {
        ast_t *ast = parse_toplevel(&p);

        if (! ast) {
            if (parse_eof(&p)) break;
            if (parsed_semicolon(&p)) continue;

            fprintf(stderr, "==========================\n PARSER ERROR\n\n");
        }

        // 
        switch (ast->type) {
          case AST_FUNDEF: {
            Function *func = codegenFunDef(cg, ast);
            if (func) {
                printf("OK\n");
            }
            else fprintf(stderr, "test_interp: codegenFunDef() failed\n");
          } break;

          default: {
            ast_t void_proto = { 
                .type = AST_FUNDEF, 
                .as_fundef = { .name = "", .args = 0, .body = ast }
            };
            Function *func = codegenFunDef(cg, &void_proto);
            if (func) {
                void *funcptr = theEngine->getPointerToFunction(func);
                doublefunc_t fp = (doublefunc_t) funcptr;
                double result = fp();
                fprintf(stderr, "OK: %f\n", result);
            } else 
                fprintf(stderr, "test_interp: codegen() failed\n");
          } 
        }

        free_ast(ast);
        printf("#KLD# ");
    }
}

}
