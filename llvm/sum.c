#include <stdlib.h>
#include <stdio.h>
#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>

/*
 *  Source:
 *  https://pauladamsmith.com/blog/2015/01/how-to-get-started-with-llvm-c-api.html
 */

/*
 * Compilation and linking:
 *
 * $ clang `llvm-config-mp-3.5 --cflags` -g -c sum.c -o sum.o
 * $ clang++ sum.o \
 *     `llvm-config-mp-3.5 --cxxflags --ldflags \
 *      --libs core executionengine jit interpreter analysis native bitwriter \
 *      --system-libs` -o sum
 *
 */

int main(int argc, char const *argv[]) {
    LLVMModuleRef mod = LLVMModuleCreateWithName("sum");

    LLVMTypeRef param_types[] = { LLVMInt32Type(), LLVMInt32Type() };
    LLVMTypeRef ret_type = LLVMFunctionType(LLVMInt32Type(), param_types, 2, 0);
    LLVMValueRef sum = LLVMAddFunction(mod, "sum", ret_type);

    LLVMBasicBlockRef entry = LLVMAppendBasicBlock(sum, "entry");
    
    LLVMBuilderRef builder = LLVMCreateBuilder();
    LLVMPositionBuilderAtEnd(builder, entry);
    LLVMValueRef tmp = LLVMBuildAdd(builder, 
                            LLVMGetParam(sum, 0), 
                            LLVMGetParam(sum, 1), "tmp");
    LLVMBuildRet(builder, tmp);

    char *error = NULL;
    LLVMVerifyModule(mod, LLVMAbortProcessAction, &error);
    LLVMDisposeMessage(error);

    LLVMExecutionEngineRef engine;
    error = NULL;
    LLVMLinkInJIT();
    LLVMInitializeNativeTarget();
    if (LLVMCreateExecutionEngineForModule(&engine, mod, &error) != 0) {
        fprintf(stderr, "failed to create execution engine\n");
        abort();
    }
    if (error) {
        fprintf(stderr, "error: %s\n", error);
        LLVMDisposeMessage(error);
        exit(EXIT_FAILURE);
    }

    if (argc < 3) {
        fprintf(stderr, "usage: %s x y\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    long long x = strtoll(argv[1], NULL, 10);
    long long y = strtoll(argv[2], NULL, 10);

    LLVMGenericValueRef args[] = {
        LLVMCreateGenericValueOfInt(LLVMInt32Type(), x, 0),
        LLVMCreateGenericValueOfInt(LLVMInt32Type(), y, 0),
    };
    LLVMGenericValueRef res = LLVMRunFunction(engine, sum,  2, args);
    printf("%d\n", (int)LLVMGenericValueToInt(res, 0));

    // write bitcode to file
    if (LLVMWriteBitcodeToFile(mod, "sum.bc") != 0) {
        fprintf(stderr, "error writing bitcode to file\n");
    }

    LLVMDisposeBuilder(builder);
    LLVMDisposeExecutionEngine(engine);
}
