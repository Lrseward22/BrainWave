#ifndef BRAINWAVE_SEMA_SEMA_H
#define BRAINWAVE_SEMA_SEMA_H

#include <brainwave/Parser/Parser.h>
#include <brainwave/Sema/Environment.h>

class Sema {
    Parser &P;
    llvm::SmallVector<std::unique_ptr<Environment>, 256> envs;
    llvm::SmallVector<std::string, 256> builtins;

    Token createToken(const std::string& name, tok::TokenKind kind = tok::TokenKind::IDENTIFIER) {
        builtins.push_back(name);
        const char* data = name.c_str();
        return Token(data, name.length(), kind);
    }
        
    std::unique_ptr<Declare> createDeclare(const std::string& name, Ty::Type type, bool isStatic = false) {
        Token iden = createToken(name);
        auto varExpr = std::make_unique<Variable>(iden);
        varExpr->setType(type);
        return std::make_unique<Declare>(type, std::move(varExpr), isStatic, iden.getLocation());
    }

    std::unique_ptr<FunStmt> createFunctionDecl(
            const std::string& name,
            Ty::Type returnType,
            llvm::SmallVector<std::unique_ptr<Declare>, 256> params,
            bool isStatic = false
            ) {
        Token iden = createToken(name);
        std::unique_ptr<Stmt> body = nullptr;
        auto func = std::make_unique<FunStmt>(iden, std::move(params),
                returnType, std::move(body), iden.getLocation(), isStatic);

        return func;
    }

    void registerString();

    public:
    Sema(Parser &P) : P(P) { 
        Ty::initializeBuiltInTypes();
        auto env = std::make_unique<Environment>(EnvKind::Base, nullptr);
        envs.push_back(std::move(env));

        registerString();
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
