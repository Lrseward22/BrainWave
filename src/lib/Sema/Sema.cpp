#include <brainwave/Sema/Sema.h>
#include <iostream>

using namespace brainwave;

namespace {
class EnvCreation : public ASTVisitor{
    // Creates environments:
    //      Defines declared Variables
    //      Defines declared functions
    //      Creates Scopes
    brainwave::DiagnosticsEngine &Diag;
    llvm::SmallVector<std::unique_ptr<Environment>>* envs;
    Environment* baseEnv;
    Environment* currEnv;
    Token iden;

    void pushEnv(EnvKind kind) {
        auto env = std::make_unique<Environment>(kind, currEnv);
        currEnv = env.get();
        envs->push_back(std::move(env));
    }

    void popEnv() {
        currEnv = currEnv->getParent();
    }

public:
    EnvCreation(brainwave::DiagnosticsEngine &Diag, llvm::SmallVector<std::unique_ptr<Environment>>* envs)
        : Diag(Diag), envs(envs) { }

    void run(AST* Tree, Environment* base) {
        baseEnv = base;
        currEnv = baseEnv;
        Tree->accept(*this);
    }

    // Expression ASTs
    virtual void visit(BinaryOp &expr) override { 
        expr.getLeft()->accept(*this);
        expr.getRight()->accept(*this);
    };
    virtual void visit(UnaryOp &expr) override {
        expr.getExpr()->accept(*this);
    };
    virtual void visit(Grouping &expr) override {
        expr.getExpr()->accept(*this);
    };
    virtual void visit(Literal &expr) override { };
    virtual void visit(Variable &expr) override {
        iden = expr.getIdentifier();
    };
    virtual void visit(Logical &expr) override {
        expr.getLeft()->accept(*this);
        expr.getRight()->accept(*this);
    };
    virtual void visit(Assign &expr) override { 
        iden = expr.getIdentifier();
    };
    virtual void visit(FunExpr &expr) override { 
        for (const auto& p: expr.getParams())
            p->accept(*this);
    };

    // Statement ASTs
    virtual void visit(Block &stmt) override {
        pushEnv(EnvKind::Block);

        for (const auto& s: stmt.getStmts())
            s->accept(*this);

        stmt.env = currEnv;
        popEnv();
    };
    virtual void visit(Print &stmt) override { };
    virtual void visit(Read &stmt) override { };
    virtual void visit(Return &stmt) override { };
    virtual void visit(If &stmt) override { 
        pushEnv(EnvKind::If);
        stmt.getIfStmt()->accept(*this);
        stmt.ifEnv = currEnv;
        popEnv();

        if (stmt.getElseStmt()) {
            pushEnv(EnvKind::If);
            stmt.getElseStmt()->accept(*this);
            stmt.elseEnv = currEnv;
            popEnv();
        }
    };
    virtual void visit(While &stmt) override { 
        pushEnv(EnvKind::Loop);

        stmt.getStmt()->accept(*this);

        stmt.env = currEnv;
        popEnv();
    };
    virtual void visit(Until &stmt) override { 
        pushEnv(EnvKind::Loop);

        stmt.getStmt()->accept(*this);

        stmt.env = currEnv;
        popEnv();
    };
    virtual void visit(For &stmt) override { 
        pushEnv(EnvKind::Loop);

        stmt.getDecl()->accept(*this);
        stmt.getStmt()->accept(*this);

        stmt.env = currEnv;
        popEnv();
    };
    virtual void visit(FunStmt &stmt) override { 
        // Environment creation for function
        pushEnv(EnvKind::Function);
        for (const auto& s : stmt.getParams())
            s->accept(*this);
        stmt.getBody()->accept(*this);
        stmt.env = currEnv;
        popEnv();

        // Register function in base environment
        if (currEnv->defineFunc(stmt.getIdentifier().getIdentifier(), &stmt))
            Diag.report(stmt.getIdentifier().getLocation(), 
                        diag::err_func_redeclaration, 
                        stmt.getIdentifier().getIdentifier());
    };
    virtual void visit(ClassStmt &stmt) override { 
        pushEnv(EnvKind::Class);

        stmt.env = currEnv;
        popEnv();
    };
    virtual void visit(Import &stmt) override { };
    virtual void visit(Declare &stmt) override { 
        llvm::StringRef type = stmt.getType().getLexeme();
        stmt.getExpr()->accept(*this);
        if (currEnv->defineVar(iden.getIdentifier(), type))
            Diag.report(iden.getLocation(), 
                        diag::err_iden_redeclaration, 
                        iden.getIdentifier());
    };
    virtual void visit(ExprStmt &stmt) override { 
        stmt.getExpr()->accept(*this);
    };
};

class TypeChecker : public ASTVisitor{
    // Detects:
    //      Expression types matching
    //      Operator types match
    //      Function call parameter types match declaration types
    //      Return statement types match
    //      Assignments match variable types
    brainwave::DiagnosticsEngine &Diag;

public:
    TypeChecker(brainwave::DiagnosticsEngine &Diag) : Diag(Diag) { }

    void run(AST* Tree) {
        Tree->accept(*this);
    }

    // Expression ASTs
    virtual void visit(BinaryOp &expr) override { };
    virtual void visit(UnaryOp &expr) override { };
    virtual void visit(Grouping &expr) override { };
    virtual void visit(Literal &expr) override { };
    virtual void visit(Variable &expr) override { };
    virtual void visit(Logical &expr) override { };
    virtual void visit(Assign &expr) override { };
    virtual void visit(FunExpr &expr) override { };

    // Statement ASTs
    virtual void visit(Block &stmt) override { };
    virtual void visit(Print &stmt) override { };
    virtual void visit(Read &stmt) override { };
    virtual void visit(Return &stmt) override { };
    virtual void visit(If &stmt) override { };
    virtual void visit(While &stmt) override { };
    virtual void visit(Until &stmt) override { };
    virtual void visit(For &stmt) override { };
    virtual void visit(FunStmt &stmt) override { };
    virtual void visit(ClassStmt &stmt) override { };
    virtual void visit(Import &stmt) override { };
    virtual void visit(Declare &stmt) override { };
    virtual void visit(ExprStmt &stmt) override { };
};

class ScopeResolution : public ASTVisitor{
    // Detects:
    //      undeclared variables,
    //      undeclared functions,
    brainwave::DiagnosticsEngine &Diag;
    Environment* env;

public:
    ScopeResolution(brainwave::DiagnosticsEngine &Diag) : Diag(Diag) { }

    void run(AST* Tree, Environment* e) {
        env = e;
        Tree->accept(*this);
    }

    // Expression ASTs
    virtual void visit(BinaryOp &expr) override { 
        expr.getLeft()->accept(*this);
        expr.getRight()->accept(*this);
    };
    virtual void visit(UnaryOp &expr) override {
        expr.getExpr()->accept(*this);
    };
    virtual void visit(Grouping &expr) override {
        expr.getExpr()->accept(*this);
    };
    virtual void visit(Literal &expr) override { };
    virtual void visit(Variable &expr) override {
        if (env->getVar(expr.getIdentifier()) == "")
            Diag.report(expr.getIdentifier().getLocation(),
                        diag::err_iden_undeclared, 
                        expr.getData());
    };
    virtual void visit(Logical &expr) override {
        expr.getLeft()->accept(*this);
        expr.getRight()->accept(*this);
    };
    virtual void visit(Assign &expr) override {
        if (env->getVar(expr.getIdentifier()) == "")
            Diag.report(expr.getIdentifier().getLocation(),
                        diag::err_iden_undeclared, 
                        expr.getIdentifier().getIdentifier());
    };
    virtual void visit(FunExpr &expr) override { 
        FunStmt* func = env->getFunc(expr.getIdentifier().getIdentifier());
        if (func == nullptr)
            Diag.report(expr.getIdentifier().getLocation(),
                        diag::err_func_undeclared, 
                        expr.getIdentifier().getIdentifier());
        for (const auto& p: expr.getParams())
            p->accept(*this);
    };

    // Statement ASTs
    virtual void visit(Block &stmt) override {
        env = stmt.env;
        for (const auto& s: stmt.getStmts())
            s->accept(*this);
    };
    virtual void visit(Print &stmt) override { 
        stmt.getExpr()->accept(*this);
    };
    virtual void visit(Read &stmt) override {
        if (env->getVar(stmt.getIdentifier()) == "")
            Diag.report(stmt.getIdentifier().getLocation(),
                        diag::err_iden_undeclared, 
                        stmt.getIdentifier().getIdentifier());
    };
    virtual void visit(Return &stmt) override { 
        stmt.getExpr()->accept(*this);
    };
    virtual void visit(If &stmt) override { 
        env = stmt.ifEnv;
        stmt.getExpr()->accept(*this);
        stmt.getIfStmt()->accept(*this);
        if (stmt.getElseStmt()) {
            env = stmt.elseEnv;
            stmt.getElseStmt()->accept(*this);
        }
    };
    virtual void visit(While &stmt) override { 
        env = stmt.env;
        stmt.getExpr()->accept(*this);
        stmt.getStmt()->accept(*this);
    };
    virtual void visit(Until &stmt) override {
        env = stmt.env;
        stmt.getExpr()->accept(*this);
        stmt.getStmt()->accept(*this);
    };
    virtual void visit(For &stmt) override {
        env = stmt.env;
        stmt.getCond()->accept(*this);
        stmt.getUpdate()->accept(*this);
        stmt.getStmt()->accept(*this);
    };
    virtual void visit(FunStmt &stmt) override {
        env = stmt.env;
        stmt.getBody()->accept(*this);
    };
    virtual void visit(ClassStmt &stmt) override { };
    virtual void visit(Import &stmt) override { };
    virtual void visit(Declare &stmt) override {
        stmt.getExpr()->accept(*this);
    };
    virtual void visit(ExprStmt &stmt) override {
        stmt.getExpr()->accept(*this);
    };
};

class ControlFlow : public ASTVisitor{
    // Detects:
    //      Break/Continue inside loop
    //      Return inside function
    //      All paths return value
    //      Optional: Unreachable code
    brainwave::DiagnosticsEngine &Diag;

public:
    ControlFlow(brainwave::DiagnosticsEngine &Diag) : Diag(Diag) { }

    void run(AST* Tree) {
        Tree->accept(*this);
    }

    // Expression ASTs
    virtual void visit(BinaryOp &expr) override { };
    virtual void visit(UnaryOp &expr) override { };
    virtual void visit(Grouping &expr) override { };
    virtual void visit(Literal &expr) override { };
    virtual void visit(Variable &expr) override { };
    virtual void visit(Logical &expr) override { };
    virtual void visit(Assign &expr) override { };
    virtual void visit(FunExpr &expr) override { };

    // Statement ASTs
    virtual void visit(Block &stmt) override { };
    virtual void visit(Print &stmt) override { };
    virtual void visit(Read &stmt) override { };
    virtual void visit(Return &stmt) override { };
    virtual void visit(If &stmt) override { };
    virtual void visit(While &stmt) override { };
    virtual void visit(Until &stmt) override { };
    virtual void visit(For &stmt) override { };
    virtual void visit(FunStmt &stmt) override { };
    virtual void visit(ClassStmt &stmt) override { };
    virtual void visit(Import &stmt) override { };
    virtual void visit(Declare &stmt) override { };
    virtual void visit(ExprStmt &stmt) override { };
};
}

// Other optional passes:
//      Constants Folding
//      Definite Assignment: Initialized variable before use

std::unique_ptr<AST> Sema::next() {
    // Create all semantic pass visitors
    // get next ast
    // feed it into all passes
    std::unique_ptr<AST> ast = std::move(P.parse());
    EnvCreation EnvC(getDiagnostics(), &envs);
    ScopeResolution ScoRe(getDiagnostics());
    while (ast) {
        ast->print();
        std::cout << "\n";
        EnvC.run(ast.get(), currEnv);
        ScoRe.run(ast.get(), currEnv);
        ast = P.parse();
    }
    return std::move(ast);
}
