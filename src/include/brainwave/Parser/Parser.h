#ifndef BRAINWAVE_PARSER_PARSER_H
#define BRAINWAVE_PARSER_PARSER_H

#include "AST.h"
#include <brainwave/Parser/AST.h>
#include <brainwave/Lexer/Lexer.h>
#include "llvm/Support/raw_ostream.h"
#include <iostream>

class Parser {
    Lexer &Lex;
    Token Tok;
    bool HasError;

    void panic() {
        expect(tok::SEMI);
        while (!match(tok::SEMI) && !atEnd())
            advance();
    }

    void advance() { Lex.next(Tok); }

    bool expect(tok::TokenKind Kind) {
        if (Tok.getKind() != Kind) {
            std::string received;
            std::string expected;
            received = tok::formatTokenKind(Tok.getKind());
            expected = tok::formatTokenKind(Kind);
            getDiagnostics().report(Tok.getLocation(), diag::err_unexpected_token, 
                    received, expected);
            return true;
        }
        return false;
    }

    bool match(tok::TokenKind Kind) {
        return (Kind == Tok.getKind());
    }

    bool atEnd() {
        return (Tok.getKind() == tok::TokenKind::EOI);
    }

    bool consume(tok::TokenKind Kind) {
        if (expect(Kind))
            return true;
        advance();
        return false;
    }

    tok::TokenKind peek() {
        return Lex.peek();
    }

    std::unique_ptr<AST> parseBrainWave();

    // EXPRs
    std::unique_ptr<Expr> parseExpression();
    std::unique_ptr<Expr> parseAssign();
    std::unique_ptr<Expr> parseLogical();
    std::unique_ptr<Expr> parseBoolExpr();
    std::unique_ptr<Expr> parseExpr();
    std::unique_ptr<Expr> parseTermExpr();
    std::unique_ptr<Expr> parseFactorExpr();
    std::unique_ptr<Expr> parseUnary();
    std::unique_ptr<Expr> parseCast();
    std::unique_ptr<Expr> parseMemberExpr();
    std::unique_ptr<Expr> parseGrouping();
    std::unique_ptr<Expr> parseBaseExpr();
    std::unique_ptr<Expr> parseFunExpr(Token identifier);
    
    // STMTs
    std::unique_ptr<Stmt> parseStmt();
    std::unique_ptr<Stmt> parseExprStmt();
    std::unique_ptr<Stmt> parseDeclare();
    std::unique_ptr<Stmt> parseDeclareStmt();
    std::unique_ptr<Stmt> parseBlock();
    std::unique_ptr<Stmt> parsePrint();
    std::unique_ptr<Stmt> parseRead();
    std::unique_ptr<Stmt> parseReturn();
    std::unique_ptr<Stmt> parseBreak();
    std::unique_ptr<Stmt> parseContinue();
    std::unique_ptr<Stmt> parseIf();
    std::unique_ptr<Stmt> parseWhile();
    std::unique_ptr<Stmt> parseUntil();
    std::unique_ptr<Stmt> parseFor();
    std::unique_ptr<Stmt> parseFunStmt();
    std::unique_ptr<Stmt> parseClass();
    std::unique_ptr<FunStmt> parseConstructor();
    std::unique_ptr<Stmt> parseImport();

    void unmatchedCharError(Token tok) {
        std::string ch = tok::formatTokenKind(tok.getKind());
        getDiagnostics().report(tok.getLocation(), diag::err_unmatched_char, ch);
    }

    void InvalidExprError() {
        getDiagnostics().report(Tok.getLocation(), diag::err_invalid_expr);
    }

    void FunReturnError() {
        getDiagnostics().report(Tok.getLocation(), diag::err_invalid_return_type);
    }

    public:
    Parser(Lexer &Lex) : Lex(Lex), HasError(false) {
        Tok = Token();
        advance();
    }

    brainwave::DiagnosticsEngine &getDiagnostics() const {
        return Lex.getDiagnostics();
    }

    bool hasError() { return HasError; }

    std::unique_ptr<AST> parse();
};

#endif
