#ifndef BRAINWAVE_SEMA_SEMA_H
#define BRAINWAVE_SEMA_SEMA_H

#include <brainwave/Parser/Parser.h>
#include <brainwave/Sema/Environment.h>

class Sema {
    Parser &P;
    std::unique_ptr<AST> ast;
    Environment env;
    Environment *currEnv;

    brainwave::DiagnosticsEngine &getDiagnostics() const {
        return P.getDiagnostics();
    }

    public:
    Sema(Parser &P) : P(P), env(Environment(EnvKind::Base, nullptr)) {
        currEnv = &env;
    }

    std::unique_ptr<AST> next();

};

#endif
