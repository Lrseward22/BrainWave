//#include <brainwave/Utils/Diagnostics.h>
#include <brainwave/Sema/Sema.h>
//#include <brainwave/Generator/CodeGen.h>
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Support/TargetSelect.h"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/TargetParser/Host.h"

#include "llvm/CodeGen/CommandFlags.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Pass.h"
#include "llvm/Target/TargetMachine.h"
#include <iostream>

using namespace brainwave;

// Command Line Options
static llvm::cl::opt<std::string> MTriple("mtriple", llvm::cl::desc("Override target triple for module"));
static llvm::cl::opt<bool> EmitLLVM(
        "emit-llvm",
        llvm::cl::desc("Emit IR code instead of assembler"),
        llvm::cl::init(false));
static llvm::cl::opt<bool> EmitAsm(
        "S",
        llvm::cl::desc("Emit Assembly code (.s)"),
        llvm::cl::init(false));
static llvm::cl::opt<bool> EmitObj(
        "c",
        llvm::cl::desc("Emit Object code (.o)"),
        llvm::cl::init(false));
static llvm::cl::list<std::string> InputFiles(
        llvm::cl::Positional,
        llvm::cl::desc("<input files>"),
        llvm::cl::OneOrMore);
static llvm::cl::opt<std::string> OutputFilename(
        "o", llvm::cl::desc("Output filename"),
        llvm::cl::value_desc("filename"));
static llvm::CodeGenFileType FileType;

llvm::TargetMachine* createTargetMachine(const char* Argv0) {
    llvm::Triple Triple = llvm::Triple(
            !MTriple.empty()
                ? llvm::Triple::normalize(MTriple)
                : llvm::sys::getDefaultTargetTriple());

    //std::cout << Triple.str() << std::endl;

    //llvm::TargetOptions TargetOptions =
    //    llvm::codegen::InitTargetOptionsFromCodeGenFlags(Triple);
    llvm::TargetOptions TargetOptions;
    std::string CPUStr = llvm::codegen::getCPUStr();
    std::string FeatureStr = llvm::codegen::getFeaturesStr();
    std::string Error;
    const llvm::Target* Target = 
        llvm::TargetRegistry::lookupTarget(
                llvm::codegen::getMArch(), Triple, Error);
    if (!Target) {
        llvm::WithColor::error(llvm::errs(), Argv0) << Error;
        return nullptr;
    }

    std::optional<llvm::Reloc::Model> RelocModel = llvm::codegen::getRelocModel();
    if (!llvm::codegen::getExplicitRelocModel()) 
        RelocModel = llvm::Reloc::PIC_;
    llvm::TargetMachine* TM = Target->createTargetMachine(
            Triple.getTriple(), CPUStr, FeatureStr,
            TargetOptions, RelocModel,
            llvm::CodeModel::Small);
    return TM;
}

bool emit(llvm::StringRef Argv0, llvm::Module *M, 
        llvm::TargetMachine *TM, llvm::StringRef InputFilename) {
    if (OutputFilename.empty()) {
        if (InputFilename == "-")
            OutputFilename = "-";
        else {
            if (InputFilename.endswith(".bw"))
                OutputFilename = InputFilename.drop_back(3).str();
            else {
                llvm::WithColor::error(llvm::errs(), Argv0) 
                    << "File Extension not supported\n";
                return false;
            }
            switch (FileType) {
                case llvm::CGFT_AssemblyFile:
                    OutputFilename.append(EmitLLVM ? ".ll" : ".s");
                    break;
                case llvm::CGFT_ObjectFile:
                    OutputFilename.append(".o");
                    break;
                case llvm::CGFT_Null:
                    OutputFilename.append(".null");
                    break;
            }
        }
    }
    std::error_code EC;
    llvm::sys::fs::OpenFlags OpenFlags = static_cast<llvm::sys::fs::OpenFlags>(0);
    auto Out = std::make_unique<llvm::ToolOutputFile>(
            OutputFilename, EC, OpenFlags);
    if (EC) {
        llvm::WithColor::error(llvm::errs(), Argv0) << EC.message() << '\n';
        return false;
    }
    llvm::legacy::PassManager PM;
    if (FileType == llvm::CGFT_AssemblyFile && EmitLLVM)
        PM.add(createPrintModulePass(Out->os()));
    else
        if (TM->addPassesToEmitFile(PM, Out->os(), nullptr, FileType)) {
            llvm::WithColor::error(llvm::errs(), Argv0) << "No support for file type\n";
            return false;
        }
    PM.run(*M);
    Out->keep();
    return true;
}

bool linkExecutable(llvm::StringRef Argv0, llvm::StringRef ObjectFile, llvm::StringRef OutputFile) {
    // Use system linker (gcc or clang)
    //std::string LinkerCmd = "gcc -no-pie ";
    std::string LinkerCmd = "gcc ";
    LinkerCmd += ObjectFile.str();
    LinkerCmd += " -o ";
    LinkerCmd += OutputFile.str();
    
    int result = system(LinkerCmd.c_str());
    if (result != 0) {
        llvm::WithColor::error(llvm::errs(), Argv0) 
            << "Linking failed with code " << result << '\n';
        return false;
    }
    
    // Optionally remove the object file
    // llvm::sys::fs::remove(ObjectFile);
    
    return true;
}

int main(int argc_, const char **argv_) {
    llvm::InitLLVM X(argc_, argv_);
    static llvm::codegen::RegisterCodeGenFlags CGF;
    llvm::outs() << "BrainWave " << "Version 1.0.0" << '\n';

    // Native targeting asm for prototyping
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    /* Production level
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();
    */

    llvm::cl::ParseCommandLineOptions(argc_, argv_, "BrainWave compiler\n");
    
    for (unsigned i = 0; i < InputFiles.size(); ++i) {
        std::string F = InputFiles[i];
        if (!llvm::StringRef(F).endswith(".bw")) {
            llvm::WithColor::error(llvm::errs(), argv_[0])
                << "Input file must have .bw extension: " << F << '\n';
            continue;
        }

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
        auto TheParser = Parser(TheLexer);
        auto TheSema = Sema(TheParser);
        TheSema.next();
        //auto TheGenerator = CodeGen(TheParser);

        /*
        llvm::TargetMachine* TM = createTargetMachine(argv_[0]);
        if (!TM) {
            llvm::errs() << "Failed to create the Target Machine\n";
            return 1;
        }
        TheGenerator.compile(argv_[0], F.c_str(), TM);

        llvm::Module* M = TheGenerator.getModule();
        if (!M) {
            llvm::errs() << "Failed to get module from Code Generator";
            return 1;
        }

        std::string VerifyErr;
        llvm::raw_string_ostream VerifyStream(VerifyErr);
        if (llvm::verifyModule(*M, &VerifyStream)) {
            llvm::errs() << "Module Verification Failed: " << VerifyStream.str() << '\n';
            return 1;
        }

        bool userSpecifiedOutput = EmitLLVM || EmitAsm || EmitObj;
        if (userSpecifiedOutput) {
            if (EmitLLVM || EmitAsm)
                FileType = llvm::CGFT_AssemblyFile;
            else if (EmitObj)
                FileType = llvm::CGFT_ObjectFile;
            if (!emit(argv_[0], M, TM, F)) return 1;
        } else {
            llvm::StringRef InputRef(F);
            std::string ObjectFile = InputRef.drop_back(3).str() + ".o";

            std::string SavedOutput = OutputFilename.getValue();
            OutputFilename = ObjectFile;
            FileType = llvm::CGFT_ObjectFile;
            if (!emit(argv_[0], M, TM, F)) return 1;

            OutputFilename = SavedOutput;
            std::string ExeName;
            if (!OutputFilename.empty())
                ExeName = OutputFilename.getValue();
            else
                ExeName = InputRef.drop_back(3).str();

            if (!linkExecutable(argv_[0], ObjectFile, ExeName)) return 1;

            llvm::sys::fs::remove(ObjectFile);
        }
        */
    }

    return 0;
}
