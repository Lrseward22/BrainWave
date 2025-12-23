#ifndef BRAINWAVE_SEMA_SEMA_H
#define BRAINWAVE_SEMA_SEMA_H

#include <brainwave/Parser/Parser.h>
#include <brainwave/Sema/Environment.h>

class Sema {
    Parser &P;
    Environment env;
    Environment* currEnv;

    public:
    Sema(Parser &P) : P(P), env(Environment(EnvKind::Base, nullptr)) { 
        currEnv = &env;
    }

    ~Sema() {
        if (currEnv != &env)
            delete currEnv;
    }

    brainwave::DiagnosticsEngine &getDiagnostics() const {
        return P.getDiagnostics();
    }

    Environment* getEnvironment() {
        return &env;
    }

    Environment* getCurrEnvironment() {
        return currEnv;
    }

    std::unique_ptr<AST> next();

};

#endif
