#include <brainwave/Sema/Sema.h>
#include "llvm/Support/Casting.h"
#include <iostream>

using namespace brainwave;

// FIXME: Next steps::
//      Need to check for nested assignments in environment creation int x = y = 3;
//      Change Sema::next to skip passing functions, analyze/pass next AST
//          pass functions at the end when AST obtained from parser is null

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
    virtual void visit(Cast &expr) override {
        expr.getExpr()->accept(*this);
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
    virtual void visit(Break &stmt) override { };
    virtual void visit(Continue &stmt) override { };
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
        if (currEnv->defineFunc(stmt.getIdentifier().getIdentifier())) {
            llvm::StringRef iden = stmt.getIdentifier().getIdentifier();
            Diag.report(stmt.getIdentifier().getLocation(), 
                        diag::err_func_redeclaration, 
                        iden);
            Diag.report(baseEnv->getFunc(iden)->getLoc(),
                    diag::note_fun_declared_here);
        }
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
        if (type == "void")
            Diag.report(iden.getLocation(),
                        diag::err_void_iden,
                        iden.getIdentifier());
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
    Environment* env;
    std::string Value;
    std::string FunType;
    std::unique_ptr<Expr> ResolvedLeft;
    std::unique_ptr<Expr> ResolvedRight;

    void resolveTypes(std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs, Token op) {
        lhs->accept(*this);
        std::string lType = Value;
        rhs->accept(*this);
        std::string rType = Value;

        if (lType == "int" && rType == "float")
            lhs = std::make_unique<Cast>(std::move(lhs), "float");
        else if (lType == "int" && rType == "double")
            lhs = std::make_unique<Cast>(std::move(lhs), "double");
        else if (lType == "float" && rType == "double")
            lhs = std::make_unique<Cast>(std::move(lhs), "double");
        else if (lType == "float" && rType == "int")
            lhs = std::make_unique<Cast>(std::move(lhs), "float");
        else if (lType == "double" && rType == "int")
            lhs = std::make_unique<Cast>(std::move(lhs), "double");
        else if (lType == "double" && rType == "float")
            lhs = std::make_unique<Cast>(std::move(lhs), "double");
        else if (lType != rType) {
            Diag.report(op.getLocation(),
                        diag::err_bin_op_mismatch, 
                        op.getLexeme(), lType, rType);
        }

        ResolvedLeft = std::move(lhs);
        ResolvedRight = std::move(rhs);
    };

public:
    TypeChecker(brainwave::DiagnosticsEngine &Diag) : Diag(Diag) { }

    void run(AST* Tree, Environment* e) {
        env = e;
        Tree->accept(*this);
    }

    // Expression ASTs
    virtual void visit(BinaryOp &expr) override { 
        resolveTypes(expr.getLeftUnique(), expr.getRightUnique(), expr.getOp());
        expr.setLeft(std::move(ResolvedLeft));
        expr.setRight(std::move(ResolvedRight));
        expr.getLeft()->accept(*this);
        if (Value == "void") {
            Diag.report(expr.getOp().getLocation(),
                        diag::err_void_in_expr);
            return;
        }
        switch (expr.getOp().getKind()) {
            case tok::TokenKind::PLUS:
                if (Value != "int" && Value != "bool"
                 && Value != "double" && Value != "string")
                    Diag.report(expr.getOp().getLocation(),
                                diag::err_bin_op_mismatch, 
                                expr.getOp().getLexeme(), Value, Value);
                break;
            case tok::TokenKind::MINUS:
            case tok::TokenKind::STAR:
            case tok::TokenKind::SLASH:
            case tok::TokenKind::CARET:
                if (Value != "int" && Value != "bool" && Value != "double")
                    Diag.report(expr.getOp().getLocation(),
                                diag::err_bin_op_mismatch, 
                                expr.getOp().getLexeme(), Value, Value);
                break;
            case tok::TokenKind::PERCENT:
                if (Value != "int")
                    Diag.report(expr.getOp().getLocation(),
                                diag::err_bin_op_mismatch, 
                                expr.getOp().getLexeme(), Value, Value);
                break;
            case tok::TokenKind::EQ:
            case tok::TokenKind::NEQ:
            case tok::TokenKind::LESS:
            case tok::TokenKind::LEQ:
            case tok::TokenKind::GREATER:
            case tok::TokenKind::GEQ:
                Value = "bool";
                break;
        }
    };
    virtual void visit(UnaryOp &expr) override {
        expr.getExpr()->accept(*this);
        switch (expr.getOp().getKind()) {
            case tok::TokenKind::BANG:
                if (Value != "bool")
                    Diag.report(expr.getOp().getLocation(),
                                diag::err_una_op_mismatch, 
                                expr.getOp().getLexeme(), Value);
                break;
            case tok::TokenKind::MINUS:
            case tok::TokenKind::PLUSPLUS:
            case tok::TokenKind::MINUSMINUS:
                if (Value != "int" && Value != "float" && Value != "double")
                    Diag.report(expr.getOp().getLocation(),
                                diag::err_una_op_mismatch, 
                                expr.getOp().getLexeme(), Value);
                break;
        }
    };
    virtual void visit(Grouping &expr) override {
        expr.getExpr()->accept(*this);
    };
    virtual void visit(Literal &expr) override {
        switch (expr.getTok().getKind()) {
            case tok::TokenKind::INTEGER_LITERAL:
                Value = "int";
                break;
            case tok::TokenKind::FLOAT_LITERAL:
                Value = "double";
                break;
            case tok::TokenKind::STRING_LITERAL:
                Value = "string";
                break;
            case tok::TokenKind::kw_true:
            case tok::TokenKind::kw_false:
                Value = "bool";
                break;
        }
    };
    virtual void visit(Variable &expr) override {
        llvm::StringRef v = env->getVar(expr.getIdentifier());
        Value = v.str();
    };
    virtual void visit(Logical &expr) override {
        resolveTypes(expr.getLeftUnique(), expr.getRightUnique(), expr.getOp());
        expr.setLeft(std::move(ResolvedLeft));
        expr.setRight(std::move(ResolvedRight));
        if (Value != "bool")
            Diag.report(expr.getOp().getLocation(),
                        diag::err_bin_op_mismatch, 
                        expr.getOp().getLexeme(), Value, Value);
        Value = "bool";
    };
    virtual void visit(Assign &expr) override {
        llvm::StringRef v = env->getVar(expr.getIdentifier());
        expr.getExpr()->accept(*this);
        if (v.str() != Value || Value == "void") {
            if (Value == "int" || Value == "float" || Value == "double")
                expr.setExpr(std::make_unique<Cast>(std::move(expr.getUnique()), v.str()));
            else if (Value == "void")
                Diag.report(expr.getIdentifier().getLocation(),
                            diag::err_void_assignment);
            else
                Diag.report(expr.getIdentifier().getLocation(),
                            diag::err_var_mismatch,
                            expr.getIdentifier().getIdentifier(),
                            v, Value);
        }
        Value = v.str();
    };
    virtual void visit(FunExpr &expr) override { 
        FunStmt* func = env->getFunc(expr.getIdentifier().getIdentifier());
        if (func != nullptr) {
            if (func->getParams().size() != expr.getParams().size()) {
                Diag.report(expr.getIdentifier().getLocation(),
                            diag::err_fun_param_size, 
                            expr.getIdentifier().getIdentifier(),
                            func->getParams().size(),
                            expr.getParams().size());
                Diag.report(func->getLoc(), diag::note_fun_declared_here);
            } else {
                const auto& funcParams = func->getParams();
                Environment* funcEnv = func->env;
                auto& exprParams = expr.getParams();
                for (int i = 0; i < funcParams.size(); i++) {
                    std::string reqType = funcParams[i]->getType().getLexeme().str();
                    exprParams[i]->accept(*this);
                    if (reqType != Value) {
                        if ((Value == "int" || Value == "float" || Value == "double") &&
                            (reqType == "int" || reqType == "float" || reqType == "double"))
                            exprParams[i] = std::make_unique<Cast>(std::move(exprParams[i]), reqType);
                        else {
                            Diag.report(expr.getIdentifier().getLocation(),
                                        diag::err_fun_param_type, 
                                        i, expr.getIdentifier().getIdentifier(),
                                        reqType, Value);
                            Diag.report(func->getLoc(), diag::note_fun_declared_here);
                        }
                    }
                }
            }
            Value = func->getType().getLexeme().str();
        } else
            Value = "void";
    };
    virtual void visit(Cast &expr) override {
        Value = expr.getType();
    };

    // Statement ASTs
    virtual void visit(Block &stmt) override {
        Environment* prev = env;
        env = stmt.env;
        for (const auto& s: stmt.getStmts())
            s->accept(*this);
        env = prev;
    };
    virtual void visit(Print &stmt) override { 
        stmt.getExpr()->accept(*this);
    };
    virtual void visit(Read &stmt) override { };
    virtual void visit(Return &stmt) override { 
        if (stmt.getExpr())
            stmt.getExpr()->accept(*this);
        else Value = "void";
        if (FunType != Value)
            Diag.report(stmt.getLoc(), diag::err_bad_return_type,
                        FunType, Value);
    };
    virtual void visit(Break &stmt) override { };
    virtual void visit(Continue &stmt) override { };
    virtual void visit(If &stmt) override { 
        Environment* prev = env;
        env = stmt.ifEnv;
        stmt.getExpr()->accept(*this);
        if (Value != "bool")
            Diag.report(stmt.getLoc(), diag::err_bad_condition, "if");
        stmt.getIfStmt()->accept(*this);
        if (stmt.getElseStmt()) {
            env = stmt.elseEnv;
            stmt.getElseStmt()->accept(*this);
        }
        env = prev;
    };
    virtual void visit(While &stmt) override { 
        Environment* prev = env;
        env = stmt.env;
        stmt.getExpr()->accept(*this);
        if (Value != "bool")
            Diag.report(stmt.getLoc(), diag::err_bad_condition, "while");
        stmt.getStmt()->accept(*this);
        env = prev;
    };
    virtual void visit(Until &stmt) override {
        Environment* prev = env;
        env = stmt.env;
        stmt.getExpr()->accept(*this);
        if (Value != "bool")
            Diag.report(stmt.getLoc(), diag::err_bad_condition, "until");
        stmt.getStmt()->accept(*this);
        env = prev;
    };
    virtual void visit(For &stmt) override {
        Environment* prev = env;
        env = stmt.env;
        stmt.getDecl()->accept(*this);
        stmt.getCond()->accept(*this);
        if (Value != "bool")
            Diag.report(stmt.getLoc(), diag::err_bad_condition, "for");
        stmt.getUpdate()->accept(*this);
        stmt.getStmt()->accept(*this);
        env = prev;
    };
    virtual void visit(FunStmt &stmt) override {
        // FIXME: Might need to check that no parameters are void
        Environment* prev = env;
        env = stmt.env;
        FunType = stmt.getType().getLexeme().str();
        stmt.getBody()->accept(*this);
        env = prev;
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
    virtual void visit(Cast &expr) override {
        expr.getExpr()->accept(*this);
    };

    // Statement ASTs
    virtual void visit(Block &stmt) override {
        Environment* prev = env;
        env = stmt.env;
        for (const auto& s: stmt.getStmts())
            s->accept(*this);
        env = prev;
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
        if (stmt.getExpr())
            stmt.getExpr()->accept(*this);
    };
    virtual void visit(Break &stmt) override { };
    virtual void visit(Continue &stmt) override { };
    virtual void visit(If &stmt) override { 
        Environment* prev = env;
        env = stmt.ifEnv;
        stmt.getExpr()->accept(*this);
        stmt.getIfStmt()->accept(*this);
        if (stmt.getElseStmt()) {
            env = stmt.elseEnv;
            stmt.getElseStmt()->accept(*this);
        }
        env = prev;
    };
    virtual void visit(While &stmt) override { 
        Environment* prev = env;
        env = stmt.env;
        stmt.getExpr()->accept(*this);
        stmt.getStmt()->accept(*this);
        env = prev;
    };
    virtual void visit(Until &stmt) override {
        Environment* prev = env;
        env = stmt.env;
        stmt.getExpr()->accept(*this);
        stmt.getStmt()->accept(*this);
        env = prev;
    };
    virtual void visit(For &stmt) override {
        Environment* prev = env;
        env = stmt.env;
        stmt.getCond()->accept(*this);
        stmt.getUpdate()->accept(*this);
        stmt.getStmt()->accept(*this);
        env = prev;
    };
    virtual void visit(FunStmt &stmt) override {
        Environment* prev = env;
        env = stmt.env;
        stmt.getBody()->accept(*this);
        env = prev;
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
    Environment* env;
    bool escape = false;
    bool warned = false;

    bool envHas(Environment* e, EnvKind K) {
        if (e->getKind() == K)
            return true;
        else if (e->getParent() != nullptr)
            return envHas(e->getParent(), K);
        return false;
    }

    bool envIs(Environment* e, EnvKind K) {
        return e->getKind() == K;
    }

public:
    ControlFlow(brainwave::DiagnosticsEngine &Diag) : Diag(Diag) { }

    void run(AST* Tree, Environment* e) {
        env = e;
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
    virtual void visit(Cast &expr) override {
        expr.getExpr()->accept(*this);
    };

    // Statement ASTs
    virtual void visit(Block &stmt) override {
        Environment* prev = env;
        env = stmt.env;
        for (const auto& s: stmt.getStmts())
            s->accept(*this);
        env = prev;
        escape = false;
        warned = false;
    };
    virtual void visit(Print &stmt) override {
        if (escape && !warned) {
            Diag.report(stmt.getLoc(), diag::warn_unreachable_code);
            warned = true;
        }
    };
    virtual void visit(Read &stmt) override {
        if (escape && !warned) {
            Diag.report(stmt.getLoc(), diag::warn_unreachable_code);
            warned = true;
        }
    };
    virtual void visit(Return &stmt) override { 
        if (escape && !warned) {
            Diag.report(stmt.getLoc(), diag::warn_unreachable_code);
            warned = true;
        }
        if (!envHas(env, EnvKind::Function))
            Diag.report(stmt.getLoc(), diag::err_ret_outside_function);
        if (!envIs(env, EnvKind::Base))
            escape = true;
    };
    virtual void visit(Break &stmt) override { 
        if (escape && !warned) {
            Diag.report(stmt.getLoc(), diag::warn_unreachable_code);
            warned = true;
        }
        if (!envHas(env, EnvKind::Loop))
            Diag.report(stmt.getLoc(), diag::err_brk_outside_loop);
        if (!envIs(env, EnvKind::Base))
            escape = true;
    };
    virtual void visit(Continue &stmt) override { 
        if (escape && !warned) {
            Diag.report(stmt.getLoc(), diag::warn_unreachable_code);
            warned = true;
        }
        if (!envHas(env, EnvKind::Loop))
            Diag.report(stmt.getLoc(), diag::err_cnt_outside_loop);
        if (!envIs(env, EnvKind::Base))
            escape = true;
    };
    virtual void visit(If &stmt) override { 
        if (escape && !warned) {
            Diag.report(stmt.getLoc(), diag::warn_unreachable_code);
            warned = true;
        }
        Environment* prev = env;
        env = stmt.ifEnv;
        stmt.getIfStmt()->accept(*this);
        if (stmt.getElseStmt()) {
            env = stmt.elseEnv;
            stmt.getElseStmt()->accept(*this);
        }
        env = prev;
    };
    virtual void visit(While &stmt) override { 
        if (escape && !warned) {
            Diag.report(stmt.getLoc(), diag::warn_unreachable_code);
            warned = true;
        }
        Environment* prev = env;
        env = stmt.env;
        stmt.getStmt()->accept(*this);
        env = prev;
    };
    virtual void visit(Until &stmt) override {
        if (escape && !warned) {
            Diag.report(stmt.getLoc(), diag::warn_unreachable_code);
            warned = true;
        }
        Environment* prev = env;
        env = stmt.env;
        stmt.getStmt()->accept(*this);
        env = prev;
    };
    virtual void visit(For &stmt) override {
        if (escape && !warned) {
            Diag.report(stmt.getLoc(), diag::warn_unreachable_code);
            warned = true;
        }
        Environment* prev = env;
        env = stmt.env;
        stmt.getStmt()->accept(*this);
        env = prev;
    };
    virtual void visit(FunStmt &stmt) override {
        if (escape && !warned) {
            Diag.report(stmt.getLoc(), diag::warn_unreachable_code);
            warned = true;
        }
        Environment* prev = env;
        env = stmt.env;
        stmt.getBody()->accept(*this);
        env = prev;
    };
    virtual void visit(ClassStmt &stmt) override { };
    virtual void visit(Import &stmt) override {
        if (escape && !warned) {
            Diag.report(stmt.getLoc(), diag::warn_unreachable_code);
            warned = true;
        }
    };
    virtual void visit(Declare &stmt) override {
        if (escape && !warned) {
            Diag.report(stmt.getType().getLocation(), diag::warn_unreachable_code);
            warned = true;
        }
    };
    virtual void visit(ExprStmt &stmt) override {
        if (escape && !warned) {
            Diag.report(stmt.getLoc(), diag::warn_unreachable_code);
            warned = true;
        }
    };
};
}

// Other optional passes:
//      Constants Folding
//      Definite Assignment: Initialized variable before use

std::unique_ptr<AST> Sema::next() {
    // get next ast
    std::unique_ptr<AST> ast = std::move(P.parse());
    // Create all semantic pass visitors
    EnvCreation EnvC(getDiagnostics(), &envs);
    ScopeResolution ScoRe(getDiagnostics());
    TypeChecker TypCh(getDiagnostics());
    ControlFlow ConFl(getDiagnostics());
    while (ast) {
        ast->print();
        std::cout << "\n";
        // feed it into all passes
        EnvC.run(ast.get(), getBaseEnvironment());
        ScoRe.run(ast.get(), getBaseEnvironment());
        TypCh.run(ast.get(), getBaseEnvironment());
        ConFl.run(ast.get(), getBaseEnvironment());
        if (auto* func = llvm::dyn_cast<FunStmt>(ast.get())) {
            std::unique_ptr<FunStmt> F(static_cast<FunStmt*>(ast.release()));
            llvm::StringRef funcName = F->getIdentifier().getIdentifier();
            if (getBaseEnvironment()->getFunc(funcName) == nullptr)
                getBaseEnvironment()->attachFunc(funcName, std::move(F));
        }
        ast = P.parse();
    }
    return std::move(ast);
}
