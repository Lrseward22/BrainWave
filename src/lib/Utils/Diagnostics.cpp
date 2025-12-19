#include <brainwave/Utils/Diagnostics.h>

using namespace brainwave;

namespace {
    const char *DiagnosticText[] = {
    #define DIAG(ID, Level, Msg) Msg,
    #include <brainwave/Utils/Diagnostics.def>
    };

    llvm::SourceMgr::DiagKind DiagnosticKind[] = {
    #define DIAG(ID, Level, Msg) llvm::SourceMgr::DK_##Level,
    #include <brainwave/Utils/Diagnostics.def>
    };
} // namespace

const char *
DiagnosticsEngine::getDiagnosticText(unsigned DiagID) {
    return DiagnosticText[DiagID];
}

llvm::SourceMgr::DiagKind
DiagnosticsEngine::getDiagnosticKind(unsigned DiagID) {
    return DiagnosticKind[DiagID];
}
