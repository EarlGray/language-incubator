#define __cpluplus
#include "codegen.h"

#include <cassert>
#include <cstdarg>
#include <cstdio>
#include <unistd.h>

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
    ExecutionEngine *engine;

    codegen_t(): 
        ir_builder(getGlobalContext()), fpm(nullptr), engine(nullptr)
    {}
};

void *errorVal(const char *msg, ...) {
    va_list va;
    va_start(va, msg);
    vfprintf(stderr, msg, va);
    va_end(va);
    return nullptr;
}

typedef double (*doublefunc_t)();

Value *codegen(codegen_t &cg, ast_t *ast);
Function *codegenFunDef(codegen_t &cg, ast_t *ast);

Value *codegenBinop(codegen_t &cg, ast_t *ast) {
    assert(ast->type == AST_BINOP);
    IRBuilder<> &IR = cg.ir_builder;
    Value *lval = codegen(cg, ast->as_binop.lhs);
    Value *rval = codegen(cg, ast->as_binop.rhs);
    if (!rval || !lval)
        return (Value *)errorVal("codegen(binop): codegen for LHS/RHS failed\n");

    char op = (char)ast->as_binop.op;
    switch (op) {
      case '+': return IR.CreateFAdd(lval, rval, "addtmp");
      case '-': return IR.CreateFSub(lval, rval, "subtmp");
      case '*': return IR.CreateFMul(lval, rval, "multmp");
      case '/': return IR.CreateFDiv(lval, rval, "divtmp");
      case '<':
        lval = IR.CreateFCmpULT(lval, rval, "cmptmp");
        return IR.CreateUIToFP(lval,
                            Type::getDoubleTy(getGlobalContext()),
                            "booltmp");
      default: break;
    }

    if (binop_precedence(op)) {
        Function *callee = cg.module->getFunction(std::string("binary") + op);
        if (!callee)
            return (Value *)errorVal("codegenBinop: no function for operation %c", op);
        if (callee->arg_size() != 2)
            return (Value *)errorVal("codegenBinop: binary%c does not take 2 arguments", op);

        std::vector<Value *> argvals = { lval, rval };
        return IR.CreateCall(callee, argvals, "calltmp");
    }

    return (Value *)errorVal("codegen: unknown binop %c\n", ast->as_binop.op);
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

static FILE *extModule = NULL;

int filegetc(lexer_t *l) {
    return fgetc(extModule);
}

Value *codegenImport(codegen_t &cg, ast_t *ast) {
    assert(ast->type == AST_IMPORT);
    
    /// read the module
    // is there such file?
    char *modfile = (char *)malloc(5 + strlen(ast->as_import));
    strcpy(modfile, ast->as_import);
    strcat(modfile, ".kld");

    if (access(modfile, R_OK)) 
        return (Value *)errorVal("can't find module %s: %s\n", modfile, strerror(errno));

    extModule = fopen(modfile, "r");
    if (!extModule)
        return (Value *)errorVal("failed to open module %s\n", modfile);

    parser_t p;
    parseinit(&p, new_lexer(filegetc));
    while (1) {
        ast_t *ast = parse_toplevel(&p);
        if (!ast) {
            if (parse_eof(&p)) break;
            if (parsed_semicolon(&p)) continue;
            fprintf(stderr, "Failed to parse a toplevel expression in %s\n", modfile);
            continue;
        }

        switch (ast->type) {
          case AST_FUNDEF: {
            Function *func = codegenFunDef(cg, ast);
            if (!func) fprintf(stderr, "codegenImport: failed codegenFunDef\n");
          } break;
          default: {
            ast_t void_proto = { AST_FUNDEF, { "", NULL, ast } };
            Function *func = codegenFunDef(cg, &void_proto);
            if (func) {
                if (cg.engine) {
                    void *funcptr = cg.engine->getPointerToFunction(func);
                    doublefunc_t fp = (doublefunc_t) funcptr;
                    double result = fp();
                    fprintf(stderr, "OK: codegenImport toplevel evaluated to %f\n", result);
                }
                else fprintf(stderr, "codegenImport: %s: no engine, toplevel can't be evaluated\n", modfile);
            } else 
                fprintf(stderr, "codegenImport: failed to codegen toplevel expr in %s\n", modfile);
          }
        }

        free_ast(ast);
    }

    fclose(extModule);
    extModule = NULL;

    return (Value *)1;
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
    Function *func;
    if (ast->as_fundef.body)
        func = Function::Create(functype, Function::ExternalLinkage, funname, cg.module);
    else
        func = cast<Function>(cg.module->getOrInsertFunction(funname, functype));

    // WTF? Why is it not checked beforehand?
    if (func->getName() != funname) {
        func->eraseFromParent();
        func = cg.module->getFunction(funname);

        if (! func->empty())
            return (Function *)errorVal("codegenProto: %s redefinition\n", ast->as_fundef.name);

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

    ast_t *astbody = ast->as_fundef.body;
    if (!astbody)
        return func; // prototype

    BasicBlock *bb = BasicBlock::Create(getGlobalContext(), "entry", func);
    cg.ir_builder.SetInsertPoint(bb);

    Value *bodyval = codegen(cg, astbody);
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
      default: break;
    }
    return (Value *)errorVal("codegen(): unknown ast->type %d\n", ast->type);
}

extern "C"
double printd(double x) {
    printf("%f\n", x);
    return 0.0;
}

static int lexnext(lexer_t *l) {
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

          case AST_IMPORT: {
            if (codegenImport(cg, ast))
                fprintf(stderr, "OK: import %s", ast->as_import);
            else
                fprintf(stderr, "codegenImport(%s) failed\n", ast->as_import);
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
    printf("\n");
}


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
    cg.engine = theEngine;
    
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
            continue;
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

          case AST_IMPORT: {
            if (codegenImport(cg, ast))
                fprintf(stderr, "OK: import %s\n", ast->as_import);
            else
                fprintf(stderr, "import %s failed\n", ast->as_import);
          } break;

          default: {
            ast_t void_proto = { AST_FUNDEF, { "", 0, ast } };
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
    printf("\n");
}

