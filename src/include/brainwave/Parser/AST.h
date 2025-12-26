#ifndef BRAINWAVE_AST_AST_H
#define BRAINWAVE_AST_AST_H

#include <brainwave/Utils/Token.h>
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include <memory>
#include <iostream>
#include <string>

class AST;
class Environment;

class Expr;
class BinaryOp;
class UnaryOp;
class Grouping;
class Literal;
class Variable;
class Logical;
class Assign;
class FunExpr;
class Cast;

class Stmt;
class Block;
class Print;
class Read;
class Return;
class Break;
class Continue;
class If;
class While;
class Until;
class For;
class FunStmt;
class ClassStmt;
class Import;
class Declare;
class ExprStmt;

class ASTVisitor {
    public:
        virtual void visit(AST &) {};
        // Expression ASTs
        virtual void visit(Expr &) {};
        virtual void visit(BinaryOp &) = 0;
        virtual void visit(UnaryOp &) = 0;
        virtual void visit(Grouping &) = 0;
        virtual void visit(Literal &) = 0;
        virtual void visit(Variable &) = 0;
        virtual void visit(Logical &) = 0;
        virtual void visit(Assign &) = 0;
        virtual void visit(FunExpr &) = 0;
        virtual void visit(Cast &) = 0;

        // Statement ASTs
        virtual void visit(Stmt &) {};
        virtual void visit(Block &) = 0;
        virtual void visit(Print &) = 0;
        virtual void visit(Read &) = 0;
        virtual void visit(Return &) = 0;
        virtual void visit(Break &) = 0;
        virtual void visit(Continue &) = 0;
        virtual void visit(If &) = 0;
        virtual void visit(While &) = 0;
        virtual void visit(Until &) = 0;
        virtual void visit(For &) = 0;
        virtual void visit(FunStmt &) = 0;
        virtual void visit(ClassStmt &) = 0;
        virtual void visit(Import &) = 0;
        virtual void visit(Declare &) = 0;
        virtual void visit(ExprStmt &) = 0;
};

class AST {
public:
    enum Kind {
        FunKind, ClassKind, StmtKind, ExprKind
    };

private:
    Kind K;
    
public:
    AST(Kind K) : K(K) { }
    virtual ~AST() {}
    virtual void accept(ASTVisitor &V) = 0;
    Kind getKind() const { return K; }
    virtual void print(int indent = 0) = 0;
};

class Expr : public AST {
    public:
        Expr(Kind K) : AST(K) {}
};

// TermExpr, FactorExpr, BoolExpr
class BinaryOp : public Expr {
    std::unique_ptr<Expr> left;
    Token op;
    std::unique_ptr<Expr> right;

    public:
        BinaryOp(std::unique_ptr<Expr> left, const Token& op, std::unique_ptr<Expr> right) 
            : Expr(ExprKind), left(std::move(left)), op(op), right(std::move(right)) {}
        Expr* getLeft() { return left.get(); }
        Expr* getRight() { return right.get(); }
        std::unique_ptr<Expr> getLeftUnique() { return std::move(left); }
        std::unique_ptr<Expr> getRightUnique() { return std::move(right); }
        void setLeft(std::unique_ptr<Expr> l) { left = std::move(l); }
        void setRight(std::unique_ptr<Expr> r) { right = std::move(r); }
        Token getOp() { return op; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == ExprKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Binary Op: (" << op.getLexeme().str() << ")" << std::endl;
            if (left) left->print(indent + 2);
            if (right) right->print(indent + 2);
        }
};

class UnaryOp : public Expr {
    Token op;
    std::unique_ptr<Expr> expr;

    public:
        UnaryOp(const Token& op, std::unique_ptr<Expr> expr)
            : Expr(ExprKind), op(op), expr(std::move(expr)) {}
        Token getOp() { return op; }
        Expr* getExpr() { return expr.get(); }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == ExprKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Unary Op: (" << op.getLexeme().str() << ")" << std::endl;
            if (expr) expr->print(indent + 2);
        }
};

class Grouping : public Expr {
    std::unique_ptr<Expr> expr;

    public:
        Grouping(std::unique_ptr<Expr> expr) : Expr(ExprKind), expr(std::move(expr)) {}
        Expr* getExpr() { return expr.get(); }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == ExprKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Grouping:" << std::endl;
            if (expr) expr->print(indent + 2);
        }
};

class Literal : public Expr {
    Token literal;

    public:
        Literal(const Token& tok) : Expr(ExprKind), literal(tok) {}
        Token getTok() { return literal; }
        llvm::StringRef getData() { return literal.getLiteralData(); }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == ExprKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Literal: (" << literal.getLiteralData().str() << ')' << std::endl;
        }
};

class Variable : public Expr {
    Token identifier;

    public:
        Variable(const Token& tok) : Expr(ExprKind), identifier(tok) {}
        Token getIdentifier() { return identifier; }
        llvm::StringRef getData() { return identifier.getIdentifier(); }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == ExprKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Identifier: (" << identifier.getIdentifier().str() << ')' << std::endl;
        }
};

class Logical : public Expr {
    std::unique_ptr<Expr> left;
    Token op;
    std::unique_ptr<Expr> right;

    public:
        Logical(std::unique_ptr<Expr> left, const Token& op, std::unique_ptr<Expr> right) 
            : Expr(ExprKind), left(std::move(left)), op(op), right(std::move(right)) {}
        Expr* getLeft() { return left.get(); }
        Expr* getRight() { return right.get(); }
        std::unique_ptr<Expr> getLeftUnique() { return std::move(left); }
        std::unique_ptr<Expr> getRightUnique() { return std::move(right); }
        void setLeft(std::unique_ptr<Expr> l) { left = std::move(l); }
        void setRight(std::unique_ptr<Expr> r) { right = std::move(r); }
        Token getOp() { return op; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == ExprKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Logical: (" << op.getLexeme().str() << ")" << std::endl;
            if (left) left->print(indent + 2);
            if (right) right->print(indent + 2);
        }
};

class Assign : public Expr {
    Token identifier;
    Token op;
    std::unique_ptr<Expr> expr;

    public:
        Assign(Token identifier, const Token& op, std::unique_ptr<Expr> expr)
            : Expr(ExprKind), identifier(identifier), op(op), expr(std::move(expr)) {}
        Token getIdentifier() { return identifier; }
        Token getOp() { return op; }
        Expr* getExpr() { return expr.get(); }
        std::unique_ptr<Expr> getUnique() { return std::move(expr); }
        void setExpr(std::unique_ptr<Expr> e) { expr = std::move(e); }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == ExprKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Assign: (" << identifier.getIdentifier().str() << ")" << std::endl;
            std::cout << std::string(indent + 2, ' ') << "Op: (" << op.getLexeme().str() << ")" << std::endl;
            if (expr) expr->print(indent + 2);
        }
};

class FunExpr : public Expr {
    Token identifier;
    llvm::SmallVector<std::unique_ptr<Expr>, 256> params;

    public:
        FunExpr(const Token& tok, llvm::SmallVector<std::unique_ptr<Expr>, 256> &paramslst)
            : Expr(ExprKind), identifier(tok), params(std::move(paramslst)) {}
        Token getIdentifier() { return identifier; }
        llvm::SmallVector<std::unique_ptr<Expr>, 256> &getParams() { return params; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == ExprKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Identifier: (" << identifier.getIdentifier().str() << ")" << std::endl;
            for (auto& param : params)
                param->print(indent+2);
        }
};

class Cast : public Expr {
    std::unique_ptr<Expr> Value;
    std::string type;

    public:
        Cast(std::unique_ptr<Expr> expr, std::string type)
            : Expr(ExprKind), Value(std::move(expr)), type(type) {}
        Expr* getExpr() { return Value.get(); }
        std::string getType() { return type; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == ExprKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Cast:" << std::endl;
            if (Value) Value->print(indent + 2);
            std::cout << std::string(indent+2, ' ') << "to " << type << std::endl;
        }
};

class Stmt : public AST {
    public:
        Stmt(Kind K) : AST(K) {}
};

class Block : public Stmt {
    public:
    Environment* env;
    private:
    llvm::SmallVector<std::unique_ptr<Stmt>, 256> stmts;

    public:
        Block(llvm::SmallVector<std::unique_ptr<Stmt>, 256> &stmtlst) 
            : Stmt(StmtKind), stmts(std::move(stmtlst)) {}
        llvm::SmallVector<std::unique_ptr<Stmt>, 256> &getStmts() { return stmts; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == StmtKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Block: " << std::endl;
            for (const auto& stmt : stmts) {
                if (stmt)
                    stmt->print(indent + 2);
            }
        }
};

class Declare : public Stmt {
    Token type;
    std::unique_ptr<Expr> expr;

    public:
        Declare(const Token& type, std::unique_ptr<Expr> expr)
            : Stmt(StmtKind), type(type), expr(std::move(expr)) {}
        Token getType() { return type; }
        Expr* getExpr() { return expr.get(); }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == StmtKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Declare: (" << type.getLexeme().str() << ")\n";
            if (expr) expr->print(indent + 2);
        }
};

class Print : public Stmt {
    std::unique_ptr<Expr> expr;
    llvm::SMLoc loc;

    public:
        Print(std::unique_ptr<Expr> expr, llvm::SMLoc loc)
            : Stmt(StmtKind), expr(std::move(expr)), loc(loc) {}
        Expr* getExpr() { return expr.get(); }
        llvm::SMLoc getLoc() { return loc; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == StmtKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Print: " << std::endl;
            if (expr) expr->print(indent + 2);
        }
};

class Read : public Stmt {
    Token identifier;
    llvm::SMLoc loc;

    public:
        Read(const Token& identifier, llvm::SMLoc loc)
            : Stmt(StmtKind), identifier(identifier), loc(loc) {}
        Token getIdentifier() { return identifier; }
        llvm::SMLoc getLoc() { return loc; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == StmtKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Read: " << identifier.getLexeme().str() << std::endl;
        }
};

class Return : public Stmt {
    std::unique_ptr<Expr> expr;
    llvm::SMLoc loc;

    public:
        Return(std::unique_ptr<Expr> expr, llvm::SMLoc loc)
            : Stmt(StmtKind), expr(std::move(expr)), loc(loc) {}
        Expr* getExpr() { return expr.get(); }
        llvm::SMLoc getLoc() { return loc; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == StmtKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Return: " << std::endl;
            if (expr) expr->print(indent + 2);
        }
};

class Break : public Stmt {
    llvm::SMLoc loc;

    public:
        Break(llvm::SMLoc loc) : Stmt(StmtKind), loc(loc) {}
        llvm::SMLoc getLoc() { return loc; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == StmtKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Break: " << std::endl;
        }
};

class Continue : public Stmt {
    llvm::SMLoc loc;

    public:
        Continue(llvm::SMLoc loc) : Stmt(StmtKind), loc(loc) {}
        llvm::SMLoc getLoc() { return loc; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == StmtKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Continue: " << std::endl;
        }
};

class If : public Stmt {
    public:
    Environment* ifEnv;
    Environment* elseEnv;
    private:
    std::unique_ptr<Expr> expr;
    std::unique_ptr<Stmt> ifStmt;
    std::unique_ptr<Stmt> elseStmt;
    llvm::SMLoc loc;

    public:
        If(std::unique_ptr<Expr> expr, std::unique_ptr<Stmt> ifStmt,
                std::unique_ptr<Stmt> elseStmt, llvm::SMLoc loc)
            : Stmt(StmtKind), expr(std::move(expr)), ifStmt(std::move(ifStmt)),
            elseStmt(std::move(elseStmt)), loc(loc) {}
        Expr* getExpr() { return expr.get(); }
        Stmt* getIfStmt() { return ifStmt.get(); }
        Stmt* getElseStmt() { return elseStmt.get(); }
        llvm::SMLoc getLoc() { return loc; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == StmtKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "IF: ";
            expr->print();
            if (ifStmt) ifStmt->print(indent + 2);
            if (elseStmt) {
                std::cout << std::string(indent, ' ') << "ELSE: " << std::endl;
                elseStmt->print(indent + 2);
            }
        }
};

class While : public Stmt {
    public:
    Environment* env;
    private:
    std::unique_ptr<Expr> expr;
    std::unique_ptr<Stmt> stmt;
    llvm::SMLoc loc;

    public:
        While(std::unique_ptr<Expr> expr, std::unique_ptr<Stmt> stmt, llvm::SMLoc loc)
            : Stmt(StmtKind), expr(std::move(expr)), stmt(std::move(stmt)), loc(loc) {}
        Expr* getExpr() { return expr.get(); }
        Stmt* getStmt() { return stmt.get(); }
        llvm::SMLoc getLoc() { return loc; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == StmtKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "While: ";
            expr->print();
            if (stmt) stmt->print(indent + 2);
        }
};

class Until : public Stmt {
    public:
    Environment* env;
    private:
    std::unique_ptr<Expr> expr;
    std::unique_ptr<Stmt> stmt;
    llvm::SMLoc loc;

    public:
        Until(std::unique_ptr<Expr> expr, std::unique_ptr<Stmt> stmt, llvm::SMLoc loc)
            : Stmt(StmtKind), expr(std::move(expr)), stmt(std::move(stmt)), loc(loc) {}
        Expr* getExpr() { return expr.get(); }
        Stmt* getStmt() { return stmt.get(); }
        llvm::SMLoc getLoc() { return loc; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == StmtKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Until: ";
            expr->print();
            if (stmt) stmt->print(indent + 2);
        }
};

class For : public Stmt {
    public:
    Environment* env;
    private:
    std::unique_ptr<Stmt> decl;
    std::unique_ptr<Expr> cond;
    std::unique_ptr<Expr> update;
    std::unique_ptr<Stmt> stmt;
    llvm::SMLoc loc;

    public:
        For(std::unique_ptr<Stmt> decl, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> change,
                std::unique_ptr<Stmt> stmt, llvm::SMLoc loc)
            : Stmt(StmtKind), decl(std::move(decl)), cond(std::move(cond)),
            update(std::move(change)), stmt(std::move(stmt)), loc(loc) {}
        Stmt* getDecl() { return decl.get(); }
        Expr* getCond() { return cond.get(); }
        Expr* getUpdate() { return update.get(); }
        Stmt* getStmt() { return stmt.get(); }
        llvm::SMLoc getLoc() { return loc; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == StmtKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "For: " << std::endl;
            if (decl) decl->print(indent+2);
            if (cond) cond->print(indent+2);
            if (update) update->print(indent+2);
            if (stmt) stmt->print(indent+2);
        }
};

class FunStmt : public Stmt {
    public:
    Environment* env;
    private:
    Token identifier;
    llvm::SmallVector<std::unique_ptr<Declare>, 256> params;
    Token type;
    std::unique_ptr<Stmt> body;
    llvm::SMLoc loc;

    public:
        FunStmt(const Token& tok, llvm::SmallVector<std::unique_ptr<Declare>, 256> &params,
                const Token& type, std::unique_ptr<Stmt> body, llvm::SMLoc loc)
            : Stmt(FunKind), identifier(tok), params(std::move(params)),
              type(type), body(std::move(body)), loc(loc) {}
        Token getIdentifier() { return identifier; }
        llvm::SmallVector<std::unique_ptr<Declare>, 256> &getParams() { return params; }
        Token getType() { return type; }
        Stmt* getBody() { return body.get(); }
        llvm::SMLoc getLoc() { return loc; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == FunKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Fun stmt: " << type.getLexeme().str() << " " << identifier.getIdentifier().str() << std::endl;
            std::cout << std::string(indent+2, ' ') << "Parameters: ";
            for (auto& param : params)
                param->print(indent+2);
            std::cout << '\n';
            if (body) body->print(indent+2);
        }
};

//TODO
class ClassStmt : public Stmt {
    Token string;
    public:
    Environment* env;
    private:

    public:
        ClassStmt(const Token& string)
            : Stmt(ClassKind), string(string) {}
        Token getString() { return string; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == ClassKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Import: " << string.getLexeme().str() << std::endl;
        }
};

class Import : public Stmt {
    Token string;
    llvm::SMLoc loc;

    public:
        Import(const Token& string, llvm::SMLoc loc)
            : Stmt(StmtKind), string(string), loc(loc) {}
        Token getString() { return string; }
        llvm::SMLoc getLoc() { return loc; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == StmtKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Import: " << string.getLexeme().str() << std::endl;
        }
};

class ExprStmt : public Stmt {
    std::unique_ptr<Expr> expr;
    llvm::SMLoc loc;

    public:
        ExprStmt(std::unique_ptr<Expr> expr, llvm::SMLoc loc)
            : Stmt(StmtKind), expr(std::move(expr)), loc(loc) {}
        Expr* getExpr() { return expr.get(); }
        llvm::SMLoc getLoc() { return loc; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        static bool classof(const AST* A) {
            return A->getKind() == StmtKind;
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Expr: \n";
            expr->print(indent + 2);
        }
};

#endif
