#include <brainwave/Sema/Sema.h>
#include <iostream>

using namespace brainwave;

namespace {
class EnvCreation : public ASTVisitor{
    Environment* baseEnv;
    Environment* currEnv;
    llvm::StringRef iden;

    void pushEnv(EnvKind kind) {
        Environment* env = new Environment(kind, currEnv);
        currEnv = env;
    }

    void popEnv() {
        currEnv = currEnv->getParent();
    }

public:
    EnvCreation(Environment* baseEnv) : baseEnv(baseEnv) { }

    void run(AST* Tree) {
        currEnv = baseEnv;
        Tree->accept(*this);
    }

    // Expression ASTs
    virtual void visit(BinaryOp &expr) override { };
    virtual void visit(UnaryOp &expr) override { };
    virtual void visit(Grouping &expr) override { };
    virtual void visit(Literal &expr) override { };
    virtual void visit(Variable &expr) override {
        iden = expr.getData();
    };
    virtual void visit(Logical &expr) override { };
    virtual void visit(Assign &expr) override { 
        iden = expr.getIdentifier().getIdentifier();
    };
    virtual void visit(FunExpr &expr) override { };

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
        pushEnv(EnvKind::Function);

        for (const auto& s : stmt.getParams())
            s->accept(*this);
        stmt.getBody()->accept(*this);

        stmt.env = currEnv;
        popEnv();
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
        currEnv->define(iden, type);
        std::cout << "Defining: " << iden.str() << " in environment: " << currEnv->getKind() << '\n';
    };
    virtual void visit(ExprStmt &stmt) override { };
};

class TypeChecker : public ASTVisitor{

public:
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

public:
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

class ControlFlow : public ASTVisitor{

public:
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

std::unique_ptr<AST> Sema::next() {
    // Create all semantic pass visitors
    // get next ast
    // feed it into all passes
    std::unique_ptr<AST> ast = std::move(P.parse());
    EnvCreation EnvC(currEnv);
    while (ast) {
        ast->print();
        std::cout << "\n";
        EnvC.run(ast.get());
        ast = P.parse();
    }
    return std::move(ast);
}
