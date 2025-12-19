#ifndef BRAINWAVE_AST_AST_H
#define BRAINWAVE_AST_AST_H

#include <brainwave/Utils/Token.h>
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include <memory>
#include <iostream>
#include <string>

class AST;

class Expr;
class BinaryOp;
class UnaryOp;
class Grouping;
class Literal;
class Variable;
class Logical;
class Assign;
class FunExpr;

class Stmt;
class Block;
class Print;
class Read;
class Return;
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
        virtual void visitExpr(Expr &) = 0;
        virtual void visitBinaryOp(BinaryOp &) = 0;
        virtual void visitUnaryOp(UnaryOp &) = 0;
        virtual void visitGrouping(Grouping &) = 0;
        virtual void visitLiteral(Literal &) = 0;
        virtual void visitVariable(Variable &) = 0;
        virtual void visitLogical(Logical &) = 0;
        virtual void visitAssign(Assign &) = 0;
        virtual void visitFunExpr(FunExpr &) = 0;

        // Statement ASTs
        virtual void visitStmt(Stmt &) = 0;
        virtual void visitBlock(Block &) = 0;
        virtual void visitPrint(Print &) = 0;
        virtual void visitRead(Read &) = 0;
        virtual void visitReturn(Return &) = 0;
        virtual void visitIf(If &) = 0;
        virtual void visitWhile(While &) = 0;
        virtual void visitUntil(Until &) = 0;
        virtual void visitFor(For &) = 0;
        virtual void visitFun(FunStmt &) = 0;
        virtual void visitClass(ClassStmt &) = 0;
        virtual void visitImport(Import &) = 0;
        virtual void visitDeclare(Declare &) = 0;
        virtual void visitExprStmt(ExprStmt &) = 0;
};

class AST {
    public:
        virtual ~AST() {}
        virtual void accept(ASTVisitor &V) = 0;
        virtual void print(int indent = 0) = 0;
};

class Expr : public AST {
    public:
        Expr() {}
};

// TermExpr, FactorExpr, BoolExpr
class BinaryOp : public Expr {
    std::unique_ptr<Expr> left;
    Token op;
    std::unique_ptr<Expr> right;

    public:
        BinaryOp(std::unique_ptr<Expr> left, const Token& op, std::unique_ptr<Expr> right) 
            : left(std::move(left)), op(op), right(std::move(right)) {}
        Expr* getLeft() { return left.get(); }
        Expr* getRight() { return right.get(); }
        Token getOp() { return op; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
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
            : op(op), expr(std::move(expr)) {}
        Token getOp() { return op; }
        Expr* getExpr() { return expr.get(); }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Unary Op: (" << op.getLexeme().str() << ")" << std::endl;
            if (expr) expr->print(indent + 2);
        }
};

class Grouping : public Expr {
    std::unique_ptr<Expr> expr;

    public:
        Grouping(std::unique_ptr<Expr> expr) : expr(std::move(expr)) {}
        Expr* getExpr() { return expr.get(); }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Grouping:" << std::endl;
            if (expr) expr->print(indent + 2);
        }
};

class Literal : public Expr {
    Token literal;

    public:
        Literal(const Token& tok) : literal(tok) {}
        llvm::StringRef getData() { return literal.getLiteralData(); }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Literal: (" << literal.getLiteralData().str() << ')' << std::endl;
        }
};

class Variable : public Expr {
    Token identifier;

    public:
        Variable(const Token& tok) : identifier(tok) {}
        llvm::StringRef getData() { return identifier.getIdentifier(); }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
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
            : left(std::move(left)), op(op), right(std::move(right)) {}
        Expr* getLeft() { return left.get(); }
        Expr* getRight() { return right.get(); }
        Token getOp() { return op; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
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
            : identifier(identifier), op(op), expr(std::move(expr)) {}
        Token getIdentifier() { return identifier; }
        Token getOp() { return op; }
        Expr* getExpr() { return expr.get(); }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
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
            : identifier(tok), params(std::move(paramslst)) {}
        llvm::SmallVector<std::unique_ptr<Expr>, 256> &getParams() { return params; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Identifier: (" << identifier.getIdentifier().str() << ")" << std::endl;
        }
};

class Stmt : public AST {
    public:
        Stmt() {}
};

class Block : public Stmt {
    llvm::SmallVector<std::unique_ptr<Stmt>, 256> stmts;

    public:
        Block(llvm::SmallVector<std::unique_ptr<Stmt>, 256> &stmtlst) 
            : stmts(std::move(stmtlst)) {}
        llvm::SmallVector<std::unique_ptr<Stmt>, 256> &getStmts() { return stmts; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Block: " << std::endl;
            for (const auto& stmt : stmts) {
                if (stmt)
                    stmt->print(indent + 2);
            }
        }
};

class Print : public Stmt {
    std::unique_ptr<Expr> expr;

    public:
        Print(std::unique_ptr<Expr> expr)
            : expr(std::move(expr)) {}
        Expr* getExpr() { return expr.get(); }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Print: " << std::endl;
            if (expr) expr->print(indent + 2);
        }
};

class Read : public Stmt {
    Token identifier;

    public:
        Read(const Token& identifier)
            : identifier(identifier) {}
        Token getIdentifier() { return identifier; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Read: " << identifier.getLexeme().str() << std::endl;
        }
};

class Return : public Stmt {
    std::unique_ptr<Expr> expr;

    public:
        Return(std::unique_ptr<Expr> expr)
            : expr(std::move(expr)) {}
        Expr* getExpr() { return expr.get(); }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Return: " << std::endl;
            if (expr) expr->print(indent + 2);
        }
};

class If : public Stmt {
    std::unique_ptr<Expr> expr;
    std::unique_ptr<Stmt> ifStmt;
    std::unique_ptr<Stmt> elseStmt;

    public:
        If(std::unique_ptr<Expr> expr, std::unique_ptr<Stmt> ifStmt, std::unique_ptr<Stmt> elseStmt)
            : expr(std::move(expr)), ifStmt(std::move(ifStmt)), elseStmt(std::move(elseStmt)) {}
        Expr* getExpr() { return expr.get(); }
        Stmt* getIf() { return ifStmt.get(); }
        Stmt* getElseStmt() { return elseStmt.get(); }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
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
    std::unique_ptr<Expr> expr;
    std::unique_ptr<Stmt> stmt;

    public:
        While(std::unique_ptr<Expr> expr, std::unique_ptr<Stmt> stmt)
            : expr(std::move(expr)), stmt(std::move(stmt)) {}
        Expr* getExpr() { return expr.get(); }
        Stmt* getStmt() { return stmt.get(); }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "While: ";
            expr->print();
            if (stmt) stmt->print(indent + 2);
        }
};

class Until : public Stmt {
    std::unique_ptr<Expr> expr;
    std::unique_ptr<Stmt> stmt;

    public:
        Until(std::unique_ptr<Expr> expr, std::unique_ptr<Stmt> stmt)
            : expr(std::move(expr)), stmt(std::move(stmt)) {}
        Expr* getExpr() { return expr.get(); }
        Stmt* getStmt() { return stmt.get(); }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Until: ";
            expr->print();
            if (stmt) stmt->print(indent + 2);
        }
};

class For : public Stmt {
    std::unique_ptr<Stmt> decl;
    std::unique_ptr<Expr> cond;
    std::unique_ptr<Expr> update;
    std::unique_ptr<Stmt> stmt;
    

    public:
        For(std::unique_ptr<Stmt> decl, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> change, std::unique_ptr<Stmt> stmt)
            : decl(std::move(decl)), cond(std::move(cond)), update(std::move(change)), stmt(std::move(stmt)) {}
        Stmt* getDecl() { return decl.get(); }
        Expr* getCond() { return cond.get(); }
        Expr* getUpdate() { return update.get(); }
        Stmt* getStmt() { return stmt.get(); }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
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
    Token identifier;
    llvm::SmallVector<std::unique_ptr<Stmt>, 256> params;
    Token type;
    std::unique_ptr<Stmt> body;

    public:
        FunStmt(const Token& tok, llvm::SmallVector<std::unique_ptr<Stmt>, 256> &params,
                const Token& type, std::unique_ptr<Stmt> body)
            : identifier(tok), params(std::move(params)), type(type), body(std::move(body)) {}
        Token getIdentifier() { return identifier; }
        llvm::SmallVector<std::unique_ptr<Stmt>, 256> &getParams() { return params; }
        Token getType() { return type; }
        Stmt* getBody() { return body.get(); }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Fun stmt: " << identifier.getIdentifier().str() << std::endl;
            std::cout << std::string(indent+2, ' ') << type.getLexeme().str() << std::endl;
            if (body) body->print(indent+2);
        }
};

//TODO
class ClassStmt : public Stmt {
    Token string;

    public:
        ClassStmt(const Token& string)
            : string(string) {}
        Token getString() { return string; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Import: " << string.getLexeme().str() << std::endl;
        }
};

class Import : public Stmt {
    Token string;

    public:
        Import(const Token& string)
            : string(string) {}
        Token getString() { return string; }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Import: " << string.getLexeme().str() << std::endl;
        }
};

class Declare : public Stmt {
    Token type;
    std::unique_ptr<Expr> expr;

    public:
        Declare(const Token& type, std::unique_ptr<Expr> expr)
            : type(type), expr(std::move(expr)) {}
        Token getType() { return type; }
        Expr* getExpr() { return expr.get(); }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Declare: (" << type.getLexeme().str() << ")\n";
            if (expr) expr->print(indent + 2);
        }
};

class ExprStmt : public Stmt {
    std::unique_ptr<Expr> expr;

    public:
        ExprStmt(std::unique_ptr<Expr> expr) : expr(std::move(expr)) {}
        Expr* getExpr() { return expr.get(); }
        virtual void accept(ASTVisitor &V) override {
            V.visit(*this);
        }
        virtual void print(int indent = 0) override {
            std::cout << std::string(indent, ' ') << "Expr: \n";
            expr->print(indent + 2);
        }
};

#endif
