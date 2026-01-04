#include <brainwave/Sema/Sema.h>
#include "llvm/Support/Casting.h"

using namespace brainwave;

namespace {
class EnvCreation : public ASTVisitor{
    // Creates environments:
    //      Defines declared Variables
    //      Defines declared functions
    //      Creates Scopes
    brainwave::DiagnosticsEngine &Diag;
    llvm::SmallVector<std::unique_ptr<Environment>, 256>* envs;
    Environment* baseEnv;
    Environment* currEnv;
    llvm::SmallVector<Token, 256> idens;

    void pushEnv(EnvKind kind) {
        auto env = std::make_unique<Environment>(kind, currEnv);
        currEnv = env.get();
        envs->push_back(std::move(env));
    }

    void popEnv() {
        currEnv = currEnv->getParent();
    }

public:
    EnvCreation(brainwave::DiagnosticsEngine &Diag, llvm::SmallVector<std::unique_ptr<Environment>, 256>* envs)
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
        idens.push_back(expr.getIdentifier());
    };
    virtual void visit(Logical &expr) override {
        expr.getLeft()->accept(*this);
        expr.getRight()->accept(*this);
    };
    virtual void visit(Assign &expr) override { 
        idens.push_back(expr.getIdentifier());
        if (auto* func = llvm::dyn_cast<Assign>(expr.getExpr()))
            expr.getExpr()->accept(*this);
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
        Ty::Type type = stmt.getType();
        stmt.getExpr()->accept(*this);
        if (type.is(Ty::TypeKind::Void))
            for (auto& iden : idens)
                Diag.report(iden.getLocation(),
                            diag::err_void_iden,
                            iden.getIdentifier());
        for (auto& iden : idens) {
            if (currEnv->defineVar(iden.getIdentifier(), type))
                Diag.report(iden.getLocation(), 
                            diag::err_iden_redeclaration, 
                            iden.getIdentifier());
        }
        idens.clear();
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
    Ty::Type Value;
    Ty::Type FunType;
    std::unique_ptr<Expr> ResolvedLeft;
    std::unique_ptr<Expr> ResolvedRight;

    void resolveTypes(std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs, Token op) {
        lhs->accept(*this);
        lhs->setType(Value);
        Ty::Type lType = Value;
        rhs->accept(*this);
        rhs->setType(Value);
        Ty::Type rType = Value;

        if (!Ty::resolvable(lType, rType))
            Diag.report(op.getLocation(),
                        diag::err_bin_op_mismatch, 
                        op.getLexeme(), lType.get(), rType.get());
        else if (resolve(lType, rType))
            rhs = std::make_unique<Cast>(std::move(rhs), lType);
        else
            lhs = std::make_unique<Cast>(std::move(lhs), rType);

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
        Value = expr.getLeft()->getType();
        expr.setType(Value);

        if (Value.is(Ty::TypeKind::Void)) {
            Diag.report(expr.getOp().getLocation(),
                        diag::err_void_in_expr);
            return;
        }
        switch (expr.getOp().getKind()) {
            case tok::TokenKind::PLUS:
                if (!Value.isNumeric() && !Value.is(Ty::TypeKind::String))
                    Diag.report(expr.getOp().getLocation(),
                                diag::err_bin_op_mismatch, 
                                expr.getOp().getLexeme(), Value.get(), Value.get());
                break;
            case tok::TokenKind::MINUS:
            case tok::TokenKind::STAR:
            case tok::TokenKind::SLASH:
            case tok::TokenKind::CARET:
                if (Value.isNumeric())
                    Diag.report(expr.getOp().getLocation(),
                                diag::err_bin_op_mismatch, 
                                expr.getOp().getLexeme(), Value.get(), Value.get());
                break;
            case tok::TokenKind::PERCENT:
                if (Value.get() != "int")
                    Diag.report(expr.getOp().getLocation(),
                                diag::err_bin_op_mismatch, 
                                expr.getOp().getLexeme(), Value.get(), Value.get());
                break;
            case tok::TokenKind::EQ:
            case tok::TokenKind::NEQ:
            case tok::TokenKind::LESS:
            case tok::TokenKind::LEQ:
            case tok::TokenKind::GREATER:
            case tok::TokenKind::GEQ:
                Value = Ty::Type("bool");
                break;
        }
    };
    virtual void visit(UnaryOp &expr) override {
        expr.getExpr()->accept(*this);
        expr.setType(Value);
        switch (expr.getOp().getKind()) {
            case tok::TokenKind::BANG:
                if (!Value.is(Ty::TypeKind::Bool))
                    Diag.report(expr.getOp().getLocation(),
                                diag::err_una_op_mismatch, 
                                expr.getOp().getLexeme(), Value.get());
                break;
            case tok::TokenKind::MINUS:
                if (!Value.isNumeric())
                    Diag.report(expr.getOp().getLocation(),
                                diag::err_una_op_mismatch, 
                                expr.getOp().getLexeme(), Value.get());
                break;
            case tok::TokenKind::PLUSPLUS:
            case tok::TokenKind::MINUSMINUS:
                // Make sure operand is variable
                if (!llvm::dyn_cast<Variable>(expr.getExpr()))
                    Diag.report(expr.getOp().getLocation(),
                                diag::err_una_assign_bad_operand,
                                expr.getOp().getLexeme());
                if (!Value.isNumeric())
                    Diag.report(expr.getOp().getLocation(),
                                diag::err_una_op_mismatch, 
                                expr.getOp().getLexeme(), Value.get());
                break;
        }
    };
    virtual void visit(Grouping &expr) override {
        expr.getExpr()->accept(*this);
        expr.setType(Value);
    };
    virtual void visit(Literal &expr) override {
        switch (expr.getTok().getKind()) {
            case tok::TokenKind::INTEGER_LITERAL:
                Value = Ty::Type("int");
                break;
            case tok::TokenKind::FLOAT_LITERAL:
                Value = Ty::Type("double");
                break;
            case tok::TokenKind::STRING_LITERAL:
                Value = Ty::Type("string");
                break;
            case tok::TokenKind::kw_true:
            case tok::TokenKind::kw_false:
                Value = Ty::Type("bool");
                break;
        }
        expr.setType(Value);
    };
    virtual void visit(Variable &expr) override {
        Value = *env->getVar(expr.getIdentifier());
        expr.setType(Value);
    };
    virtual void visit(Logical &expr) override {
        resolveTypes(expr.getLeftUnique(), expr.getRightUnique(), expr.getOp());
        expr.setLeft(std::move(ResolvedLeft));
        expr.setRight(std::move(ResolvedRight));
        if (!Value.is(Ty::TypeKind::Bool))
            Diag.report(expr.getOp().getLocation(),
                        diag::err_bin_op_mismatch, 
                        expr.getOp().getLexeme(), Value.get(), Value.get());
        Value = Ty::Type("bool");
        expr.setType(Value);
    };
    virtual void visit(Assign &expr) override {
        Ty::Type varType = *env->getVar(expr.getIdentifier());
        expr.getExpr()->accept(*this);
        if (!Ty::equals(varType, Value) || Value.is(Ty::TypeKind::Void)) {
            if (Value.isNumeric())
                expr.setExpr(std::make_unique<Cast>(std::move(expr.getUnique()), varType));
            else if (Value.is(Ty::TypeKind::Void))
                Diag.report(expr.getIdentifier().getLocation(),
                            diag::err_void_assignment);
            else
                Diag.report(expr.getIdentifier().getLocation(),
                            diag::err_var_mismatch,
                            expr.getIdentifier().getIdentifier(),
                            varType.get(), Value.get());
        }
        switch (expr.getOp().getKind()) {
            case tok::TokenKind::PLUSEQUAL:
                if (!Value.isNumeric() && !Value.is(Ty::TypeKind::String))
                    Diag.report(expr.getOp().getLocation(),
                                diag::err_bin_op_mismatch, 
                                expr.getOp().getLexeme(), varType.get(), Value.get());
                break;
            case tok::TokenKind::MINUSEQUAL:
                if (!Value.isNumeric())
                    Diag.report(expr.getOp().getLocation(),
                                diag::err_bin_op_mismatch, 
                                expr.getOp().getLexeme(), varType.get(), Value.get());
                break;
        }
        Value = varType;
        expr.setType(Value);
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
                    Ty::Type reqType = funcParams[i]->getType();
                    exprParams[i]->accept(*this);
                    if (!Ty::equals(reqType, Value)) {
                        if ((Value.isNumeric()) &&
                            (reqType.isNumeric()))
                            exprParams[i] = std::make_unique<Cast>(std::move(exprParams[i]), reqType);
                        else {
                            Diag.report(expr.getIdentifier().getLocation(),
                                        diag::err_fun_param_type, 
                                        i, expr.getIdentifier().getIdentifier(),
                                        reqType.get(), Value.get());
                            Diag.report(func->getLoc(), diag::note_fun_declared_here);
                        }
                    }
                }
            }
            Value = func->getType();
        } else
            Value = Ty::Type("void");
        expr.setType(Value);
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
        else Value = Ty::Type("void");
        if (!Ty::equals(FunType, Value))
            Diag.report(stmt.getLoc(), diag::err_bad_return_type,
                        FunType.get(), Value.get());
    };
    virtual void visit(Break &stmt) override { };
    virtual void visit(Continue &stmt) override { };
    virtual void visit(If &stmt) override { 
        Environment* prev = env;
        env = stmt.ifEnv;
        stmt.getExpr()->accept(*this);
        if (!Value.is(Ty::TypeKind::Bool))
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
        if (!Value.is(Ty::TypeKind::Bool))
            Diag.report(stmt.getLoc(), diag::err_bad_condition, "while");
        stmt.getStmt()->accept(*this);
        env = prev;
    };
    virtual void visit(Until &stmt) override {
        Environment* prev = env;
        env = stmt.env;
        stmt.getExpr()->accept(*this);
        if (!Value.is(Ty::TypeKind::Bool))
            Diag.report(stmt.getLoc(), diag::err_bad_condition, "until");
        stmt.getStmt()->accept(*this);
        env = prev;
    };
    virtual void visit(For &stmt) override {
        Environment* prev = env;
        env = stmt.env;
        stmt.getDecl()->accept(*this);
        stmt.getCond()->accept(*this);
        if (!Value.is(Ty::TypeKind::Bool))
            Diag.report(stmt.getLoc(), diag::err_bad_condition, "for");
        stmt.getUpdate()->accept(*this);
        stmt.getStmt()->accept(*this);
        env = prev;
    };
    virtual void visit(FunStmt &stmt) override {
        // FIXME: Might need to check that no parameters are void
        Environment* prev = env;
        env = stmt.env;
        FunType = stmt.getType();
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
        if (!env->getVar(expr.getIdentifier())) 
            Diag.report(expr.getIdentifier().getLocation(),
                        diag::err_iden_undeclared, 
                        expr.getData());
    };
    virtual void visit(Logical &expr) override {
        expr.getLeft()->accept(*this);
        expr.getRight()->accept(*this);
    };
    virtual void visit(Assign &expr) override {
        if (!env->getVar(expr.getIdentifier()))
            Diag.report(expr.getIdentifier().getLocation(),
                        diag::err_iden_undeclared, 
                        expr.getIdentifier().getIdentifier());
        expr.getExpr()->accept(*this);
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
        if (!env->getVar(stmt.getIdentifier()))
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
    //      Unreachable code
    brainwave::DiagnosticsEngine &Diag;
    Environment* env;
    bool escape = false;
    bool warned = false;
    bool mustReturn = false;

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
        escape = false;
        warned = false;
        mustReturn = false;
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
    virtual void visit(Cast &expr) override { };

    // Statement ASTs
    virtual void visit(Block &stmt) override {
        Environment* prev = env;
        env = stmt.env;
        for (const auto& s: stmt.getStmts())
            s->accept(*this);
        env = prev;
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
        mustReturn = true;
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
        } else if (!escape) {
            Environment* prev = env;
            bool prevMustReturn = mustReturn;
            mustReturn = false;
            env = stmt.ifEnv;
            stmt.getIfStmt()->accept(*this);
            bool ifReturns = mustReturn;
            bool elseReturns = false;
            if (stmt.getElseStmt()) {
                mustReturn = false;
                env = stmt.elseEnv;
                stmt.getElseStmt()->accept(*this);
                elseReturns = mustReturn;
            }
            env = prev;
            mustReturn = ifReturns && elseReturns;
            if (mustReturn) escape = true;
        }
    };
    virtual void visit(While &stmt) override { 
        if (escape && !warned) {
            Diag.report(stmt.getLoc(), diag::warn_unreachable_code);
            warned = true;
        } else if (!escape) {
            bool prevMustReturn = mustReturn;
            bool prevEscape = escape;
            mustReturn = false;
            escape = false;
            Environment* prev = env;
            env = stmt.env;
            stmt.getStmt()->accept(*this);
            env = prev;
            mustReturn = prevMustReturn;
            escape = prevEscape;
        }
    };
    virtual void visit(Until &stmt) override {
        if (escape && !warned) {
            Diag.report(stmt.getLoc(), diag::warn_unreachable_code);
            warned = true;
        } else if (!escape) {
            bool prevMustReturn = mustReturn;
            bool prevEscape = escape;
            mustReturn = false;
            escape = false;
            Environment* prev = env;
            env = stmt.env;
            stmt.getStmt()->accept(*this);
            env = prev;
            mustReturn = prevMustReturn;
            escape = prevEscape;
        }
    };
    virtual void visit(For &stmt) override {
        if (escape && !warned) {
            Diag.report(stmt.getLoc(), diag::warn_unreachable_code);
            warned = true;
        } else if (!escape) {
            bool prevMustReturn = mustReturn;
            bool prevEscape = escape;
            mustReturn = false;
            escape = false;
            Environment* prev = env;
            env = stmt.env;
            stmt.getStmt()->accept(*this);
            env = prev;
            mustReturn = prevMustReturn;
            escape = prevEscape;
        }
    };
    virtual void visit(FunStmt &stmt) override {
        if (escape && !warned) {
            Diag.report(stmt.getLoc(), diag::warn_unreachable_code);
            warned = true;
        }
        mustReturn = false;
        Environment* prev = env;
        env = stmt.env;
        stmt.getBody()->accept(*this);
        env = prev;
        if (!mustReturn && !stmt.getType().is(Ty::TypeKind::Void))
            Diag.report(stmt.getLoc(), diag::err_missing_return);
        mustReturn = false;
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
            Diag.report(stmt.getLoc(), diag::warn_unreachable_code);
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

class Desugar : public ASTVisitor{
    // Changes:
    //          ++ into += 1,
    //          -- into -= 1,
    //          until loops into while loops
    std::unique_ptr<Expr> ReplaceExpr = nullptr;
    std::unique_ptr<Stmt> ReplaceStmt = nullptr;

public:
    Desugar() { }

    std::unique_ptr<AST> run(std::unique_ptr<AST> Tree) {
        Tree->accept(*this);

        if (ReplaceStmt) {
            auto result = std::move(ReplaceStmt);
            ReplaceStmt = nullptr;
            return result;
        }
        return Tree;
    }

    // Expression ASTs
    virtual void visit(BinaryOp &expr) override {
        expr.getLeft()->accept(*this);
        if (ReplaceExpr) {
            expr.setLeft(std::move(ReplaceExpr));
            ReplaceExpr = nullptr;
        }
        expr.getRight()->accept(*this);
        if (ReplaceExpr) {
            expr.setRight(std::move(ReplaceExpr));
            ReplaceExpr = nullptr;
        }
    };
    virtual void visit(UnaryOp &expr) override {
        expr.getExpr()->accept(*this);
        if (ReplaceExpr) {
            expr.setExpr(std::move(ReplaceExpr));
            ReplaceExpr = nullptr;
        }

        if (expr.getOp().isOneOf(tok::TokenKind::PLUSPLUS, tok::TokenKind::MINUSMINUS)) {
            if (Variable* var = llvm::dyn_cast<Variable>(expr.getExpr())) {
                Ty::Type type = expr.getType();
                Token identifier = var->getIdentifier();

                // Create Token for Literal 1
                static const char* one = "1";
                Token oneTok(one, 1, tok::TokenKind::INTEGER_LITERAL);
                auto One = std::make_unique<Literal>(oneTok);
                One->setType(type);

                // Create Expression for variable
                auto varExpr = std::make_unique<Variable>(identifier);
                varExpr->setType(type);

                // Create Expression for var +/- 1
                tok::TokenKind opKind = (expr.getOp().getKind() == tok::TokenKind::PLUSPLUS)
                    ? tok::TokenKind::PLUS
                    : tok::TokenKind::MINUS;
                Token opTok(expr.getOp().getLexeme().data(),
                            expr.getOp().getLexeme().size(),
                            opKind);
                auto binExpr = std::make_unique<BinaryOp>(
                        std::move(varExpr), opTok, std::move(One));
                binExpr->setType(type);

                // Create = Expression
                static const char* eq = "=";
                Token eqTok(eq, 1, tok::TokenKind::EQUAL);
                ReplaceExpr = std::make_unique<Assign>(identifier, eqTok, std::move(binExpr));
                ReplaceExpr->setType(type);
            }
        }

    };
    virtual void visit(Grouping &expr) override {
        expr.getExpr()->accept(*this);
        if (ReplaceExpr) {
            expr.setExpr(std::move(ReplaceExpr));
            ReplaceExpr = nullptr;
        }
    };
    virtual void visit(Literal &expr) override { };
    virtual void visit(Variable &expr) override { };
    virtual void visit(Logical &expr) override {
        expr.getLeft()->accept(*this);
        if (ReplaceExpr) {
            expr.setLeft(std::move(ReplaceExpr));
            ReplaceExpr = nullptr;
        }
        expr.getRight()->accept(*this);
        if (ReplaceExpr) {
            expr.setRight(std::move(ReplaceExpr));
            ReplaceExpr = nullptr;
        }
    };
    virtual void visit(Assign &expr) override {
        expr.getExpr()->accept(*this);
        if (ReplaceExpr) {
            expr.setExpr(std::move(ReplaceExpr));
            ReplaceExpr = nullptr;
        }
        if (!expr.getOp().is(tok::TokenKind::EQUAL)) {
            Ty::Type type = expr.getType();
            Token identifier = expr.getIdentifier();

            // Create Expression for variable
            auto varExpr = std::make_unique<Variable>(identifier);
            varExpr->setType(type);

            // Create Expression for var +/- expr
            tok::TokenKind opKind = (expr.getOp().getKind() == tok::TokenKind::PLUSEQUAL)
                ? tok::TokenKind::PLUS
                : tok::TokenKind::MINUS;
            Token opTok(expr.getOp().getLexeme().data(),
                        expr.getOp().getLexeme().size(),
                        opKind);
            auto binExpr = std::make_unique<BinaryOp>(
                    std::move(varExpr), opTok, std::move(expr.getUnique()));
            binExpr->setType(type);

            // Create = Expression
            static const char* eq = "=";
            Token eqTok(eq, 1, tok::TokenKind::EQUAL);
            ReplaceExpr = std::make_unique<Assign>(identifier, eqTok, std::move(binExpr));
            ReplaceExpr->setType(type);
        }
    };
    virtual void visit(FunExpr &expr) override {
        for (auto& param : expr.getParams()) {
            param->accept(*this);
            if (ReplaceExpr) {
                param = std::move(ReplaceExpr);
                ReplaceExpr = nullptr;
            }
        }
    };
    virtual void visit(Cast &expr) override {
        expr.getExpr()->accept(*this);
        if (ReplaceExpr) {
            expr.setExpr(std::move(ReplaceExpr));
            ReplaceExpr = nullptr;
        }
    };

    // Statement ASTs
    virtual void visit(Block &stmt) override {
        for (auto& s : stmt.getStmts()) {
            s->accept(*this);
            if (ReplaceStmt) {
                s = std::move(ReplaceStmt);
                ReplaceStmt = nullptr;
            }
        }
    };
    virtual void visit(Print &stmt) override {
        stmt.getExpr()->accept(*this);
        if (ReplaceExpr) {
            stmt.setExpr(std::move(ReplaceExpr));
            ReplaceExpr = nullptr;
        }
    };
    virtual void visit(Read &stmt) override {
    };
    virtual void visit(Return &stmt) override {
        if (stmt.getExpr()) {
            stmt.getExpr()->accept(*this);
            if (ReplaceExpr) {
                stmt.setExpr(std::move(ReplaceExpr));
                ReplaceExpr = nullptr;
            }
        }
    };
    virtual void visit(Break &stmt) override { };
    virtual void visit(Continue &stmt) override { };
    virtual void visit(If &stmt) override {
        stmt.getExpr()->accept(*this);
        if (ReplaceExpr) {
            stmt.setExpr(std::move(ReplaceExpr));
            ReplaceExpr = nullptr;
        }
        stmt.getIfStmt()->accept(*this);
        if (ReplaceStmt) {
            stmt.setIf(std::move(ReplaceStmt));
            ReplaceStmt = nullptr;
        }

        if (stmt.getElseStmt()) {
            stmt.getElseStmt()->accept(*this);
            if (ReplaceStmt) {
                stmt.setElse(std::move(ReplaceStmt));
                ReplaceStmt = nullptr;
            }
        }
    };
    virtual void visit(While &stmt) override {
        stmt.getExpr()->accept(*this);
        if (ReplaceExpr) {
            stmt.setExpr(std::move(ReplaceExpr));
            ReplaceExpr = nullptr;
        }
        stmt.getStmt()->accept(*this);
        if (ReplaceStmt) {
            stmt.setStmt(std::move(ReplaceStmt));
            ReplaceStmt = nullptr;
        }
    };
    virtual void visit(Until &stmt) override {
        auto condExpr = stmt.getUniqueExpr();
        if (condExpr) {
            condExpr->accept(*this);
            if (ReplaceExpr) {
                condExpr = std::move(ReplaceExpr);
                ReplaceExpr = nullptr;
            }
        }
        auto bodyStmt = stmt.getUniqueStmt();
        if (bodyStmt) {
            bodyStmt->accept(*this);
            if (ReplaceStmt) {
                bodyStmt = std::move(ReplaceStmt);
                ReplaceStmt = nullptr;
            }
        }

        static const char* bang = "!";
        Token BangToken(bang, 1, tok::TokenKind::BANG);
        auto negatedCond = std::make_unique<UnaryOp>(BangToken, std::move(condExpr));
        auto whileStmt = std::make_unique<While>(std::move(negatedCond),
                                                 std::move(bodyStmt),
                                                 stmt.getLoc());
        whileStmt->env = stmt.env;
        ReplaceStmt = std::move(whileStmt);
    };
    virtual void visit(For &stmt) override {
        stmt.getDecl()->accept(*this);
        stmt.getCond()->accept(*this);
        if (ReplaceExpr) {
            stmt.setCond(std::move(ReplaceExpr));
            ReplaceStmt = nullptr;
        }
        stmt.getUpdate()->accept(*this);
        if (ReplaceExpr) {
            stmt.setUpdate(std::move(ReplaceExpr));
            ReplaceStmt = nullptr;
        }
        stmt.getStmt()->accept(*this);
        if (ReplaceStmt) {
            stmt.setStmt(std::move(ReplaceStmt));
            ReplaceStmt = nullptr;
        }
    };
    virtual void visit(FunStmt &stmt) override {
        stmt.getBody()->accept(*this);
        if (ReplaceStmt) {
            stmt.setBody(std::move(ReplaceStmt));
            ReplaceStmt = nullptr;
        }
    };
    virtual void visit(ClassStmt &stmt) override { };
    virtual void visit(Import &stmt) override {
    };
    virtual void visit(Declare &stmt) override {
        stmt.getExpr()->accept(*this);
    };
    virtual void visit(ExprStmt &stmt) override {
        stmt.getExpr()->accept(*this);
        if (ReplaceExpr) {
            stmt.setExpr(std::move(ReplaceExpr));
            ReplaceExpr = nullptr;
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
    if (!ast)
        return nullptr;
    // Create all semantic pass visitors
    EnvCreation EnvC(getDiagnostics(), &envs);
    ScopeResolution ScoRe(getDiagnostics());
    TypeChecker TypCh(getDiagnostics());
    ControlFlow ConFl(getDiagnostics());
    Desugar Des;
    // feed it into all passes
    EnvC.run(ast.get(), getBaseEnvironment());
    ScoRe.run(ast.get(), getBaseEnvironment());
    TypCh.run(ast.get(), getBaseEnvironment());
    ConFl.run(ast.get(), getBaseEnvironment());
    ast = Des.run(std::move(ast));
    return ast;
}
