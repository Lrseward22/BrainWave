#ifndef CALC_GENERATOR_CODEGEN_H
#define CALC_GENERATOR_CODEGEN_H

#include <brainwave/Sema/Sema.h>
#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Target/TargetMachine.h"

class CodeGen {
    Sema& sema;
    llvm::LLVMContext Ctx;
    std::unique_ptr<llvm::Module> M;

public:
    CodeGen(Sema &sema) : sema(sema) { }
    void compile(const char* Argv0, const char* F, llvm::TargetMachine* TM);
    llvm::Module* getModule() { return M.get(); }
};

#endif
