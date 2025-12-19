#include <brainwave/Utils/Diagnostics.h>
#include <brainwave/Sema/Sema.h>
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"
//#include <iostream>

using namespace brainwave;

int main(int argc_, const char **argv_) {
    llvm::InitLLVM X(argc_, argv_);
    llvm::SmallVector<const char *, 256> argv(argv_ + 1, argv_ + argc_);
    llvm::outs() << "BrainWave " << "Version 1.0.0" << '\n';
    
    for (const char *F: argv) {
        llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
            FileOrErr = llvm::MemoryBuffer::getFile(F);
        if (std::error_code BufferError = FileOrErr.getError()) {
            llvm::errs() << "Error reading " << F << ": " << BufferError.message() << '\n';
            continue;
        }

        llvm::SourceMgr SrcMgr;
        DiagnosticsEngine Diags(SrcMgr);
        SrcMgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());
        auto TheLexer = Lexer(SrcMgr, Diags);
        // auto TheGenerator = Generator(Diags);
        auto TheParser = Parser(TheLexer);
        //auto TheParser = Parser(TheLexer, TheGenerator);
        auto TheSema = Sema(TheParser);
        //TheParser.parse();
        TheSema.next();
    }
}
