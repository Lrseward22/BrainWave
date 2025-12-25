#ifndef BRAINWAVE_SEMA_SEMA_H
#define BRAINWAVE_SEMA_SEMA_H

#include <brainwave/Parser/Parser.h>
#include <brainwave/Sema/Environment.h>

class Sema {
    Parser &P;
    llvm::SmallVector<std::unique_ptr<Environment>> envs;

    public:
    Sema(Parser &P) : P(P) { 
        auto env = std::make_unique<Environment>(EnvKind::Base, nullptr);
        envs.push_back(std::move(env));
    }

    brainwave::DiagnosticsEngine &getDiagnostics() const {
        return P.getDiagnostics();
    }

    Environment* getBaseEnvironment() {
        return envs[0].get();
    }

    std::unique_ptr<AST> next();

};

#endif
