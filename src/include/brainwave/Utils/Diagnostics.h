#ifndef BRAINWAVE_UTILS_DIAGNOSTIC_H
#define BRAINWAVE_UTILS_DIAGNOSTIC_H

//#include "BrainWave/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include <utility>
#include <iostream>

namespace brainwave {
    namespace diag {
    enum {
    #define DIAG(ID, Level, Msg) ID,
    #include "Diagnostics.def"
    #undef DIAG
    };
    } // Namespace diag

class DiagnosticsEngine {
    static const char *getDiagnosticText(unsigned DiagID);
    static llvm::SourceMgr::DiagKind getDiagnosticKind(unsigned DiagID);

    llvm::SourceMgr &SrcMgr;
    unsigned NumErrors;

    template <typename T>
    std::string toString(const T& value) {
        std::string buffer;
        llvm::raw_string_ostream os(buffer);
        os << value;
        return os.str();
    }

    template <typename ... Args>
    std::string formatDiagnostic(const std::string& templateStr, Args&& ... args) {
        llvm::SmallVector<std::string, 4> argString{toString(std::forward<Args>(args)) ... };
        std::string result = templateStr;

        for (size_t i = 0; i < argString.size(); ++i) {
            std::string placeholder = "{" + std::to_string(i) + "}";
            size_t pos = result.find(placeholder);
            while (pos != std::string::npos) {
                result.replace(pos, placeholder.length(), argString[i]);
                pos = result.find(placeholder, pos + argString[i].length());
            }
        }

        return result;
    }

    public:
    DiagnosticsEngine(llvm::SourceMgr &SrcMgr) 
        : SrcMgr(SrcMgr), NumErrors(0) {}

    unsigned numErrors() { return NumErrors; }

    template <typename... Args>
    void report(llvm::SMLoc Loc, unsigned DiagID, Args &&... Arguments) {
        std::string Msg = 
            formatDiagnostic(getDiagnosticText(DiagID),
                          std::forward<Args>(Arguments)...);

        llvm::SourceMgr::DiagKind Kind = getDiagnosticKind(DiagID);
        SrcMgr.PrintMessage(Loc, Kind, Msg);
        NumErrors += (Kind == llvm::SourceMgr::DK_Error);
    }
};

} // Namespace brainwave

#endif
