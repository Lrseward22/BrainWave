#include <brainwave/Parser/Parser.h>
#include <iostream>
#include <brainwave/Utils/TokenKinds.h>

using namespace brainwave;

std::unique_ptr<AST> Parser::parse() {
    std::unique_ptr<Stmt> stmt;
    if (atEnd())
        return nullptr;
    stmt = std::move(parseStmt());
    //while (!atEnd()) {
    //    //std::cout << "\nToken Kind: " << tok::getTokenName(Tok.getKind()) << '\n';
    //    //std::cout << "Lexeme: " << Tok.getLexeme().str() << '\n';
    //    //advance();
    //    stmt = std::move(parseStmt());
    //    stmt->print();
    //    std::cout << "\n\n\n";
    //}
    return std::move(stmt);
}

// EXPRESSIONS

std::unique_ptr<Expr> Parser::parseExpression() {
    tok::TokenKind lookAhead = peek();
    std::unique_ptr<Expr> expr;
    if (lookAhead == tok::TokenKind::EQUAL ||
        lookAhead == tok::TokenKind::PLUSEQUAL ||
        lookAhead == tok::TokenKind::MINUSEQUAL)
        expr = parseAssign();
    else
        expr = parseLogical();
    return expr;
}

std::unique_ptr<Expr> Parser::parseAssign() {
    Token identifier = Tok;
    advance();
    Token op = Tok;
    advance();
    std::unique_ptr<Expr> expr = parseLogical();
    return std::make_unique<Assign>(identifier, op, std::move(expr));
}

std::unique_ptr<Expr> Parser::parseLogical() {
    std::unique_ptr<Expr> left = parseBoolExpr();

    while (Tok.isOneOf(tok::TokenKind::AND, tok::TokenKind::OR)) {
        Token tok = Tok;
        advance();
        std::unique_ptr<Expr> right = parseLogical();
        left = std::make_unique<Logical>(std::move(left), tok, std::move(right));
    }

    return left;
}

std::unique_ptr<Expr> Parser::parseBoolExpr() {
    std::unique_ptr<Expr> left = parseExpr();

    if (Tok.isOneOf(tok::TokenKind::EQ,
                    tok::TokenKind::NEQ,
                    tok::TokenKind::LESS, 
                    tok::TokenKind::LEQ, 
                    tok::TokenKind::GREATER, 
                    tok::TokenKind::GEQ)) {
        Token tok = Tok;
        advance();
        std::unique_ptr<Expr> right = parseExpr();
        left = std::make_unique<BinaryOp>(std::move(left), tok, std::move(right));
    }

    return left;
}

std::unique_ptr<Expr> Parser::parseExpr() {
    std::unique_ptr<Expr> left = parseTermExpr();

    while (Tok.isOneOf(tok::TokenKind::PLUS, tok::TokenKind::MINUS)) {
        Token tok = Tok;
        advance();
        std::unique_ptr<Expr> right = parseExpr();
        left = std::make_unique<BinaryOp>(std::move(left), tok, std::move(right));
    }

    return left;
}

std::unique_ptr<Expr> Parser::parseTermExpr() {
    std::unique_ptr<Expr> left = parseFactorExpr();

    while (Tok.isOneOf(tok::TokenKind::STAR, 
                tok::TokenKind::SLASH,
                tok::TokenKind::PERCENT)) {
        Token tok = Tok;
        advance();
        std::unique_ptr<Expr> right = parseTermExpr();
        left = std::make_unique<BinaryOp>(std::move(left), tok, std::move(right));
    }

    return left;
}

std::unique_ptr<Expr> Parser::parseFactorExpr() {
    std::unique_ptr<Expr> left = parseUnary();

    while (Tok.is(tok::TokenKind::CARET)) {
        Token tok = Tok;
        advance();
        std::unique_ptr<Expr> right = parseFactorExpr();
        left = std::make_unique<BinaryOp>(std::move(left), tok, std::move(right));
    }

    return left;
}

std::unique_ptr<Expr> Parser::parseUnary() {
    if (Tok.isOneOf(tok::TokenKind::BANG,
            tok::TokenKind::MINUS,
            tok::TokenKind::MINUSMINUS,
            tok::TokenKind::PLUSPLUS)) {
        Token op = Tok;
        advance();
        std::unique_ptr<Expr> expr = parseGrouping();
        return std::make_unique<UnaryOp>(op, std::move(expr));
    } else {
        std::unique_ptr<Expr> expr = parseGrouping();
        if (Tok.isOneOf(tok::TokenKind::PLUSPLUS, tok::TokenKind::MINUSMINUS)) {
            expr = std::make_unique<UnaryOp>(Tok, std::move(expr));
            advance();
        }
        return expr;
    }
}

std::unique_ptr<Expr> Parser::parseGrouping() {
    std::unique_ptr<Expr> expr;

    if (match(tok::TokenKind::L_PAREN)) {
        advance();
        if (match(tok::TokenKind::R_PAREN))
            InvalidExprError();
        else
            expr = parseLogical();
        consume(tok::TokenKind::R_PAREN);
    } else {
        expr = parseBaseExpr();
    }

    return expr;
}

std::unique_ptr<Expr> Parser::parseBaseExpr() {
    Token tok = Tok;
    advance();
    if (tok.getKind() == tok::TokenKind::IDENTIFIER) {
        if (Tok.getKind() == tok::TokenKind::L_PAREN)
            return parseFunExpr(tok);
        return std::make_unique<Variable>(tok);
    } else if (tok::isLiteral(tok.getKind()) 
            || tok.isOneOf(tok::TokenKind::kw_true, tok::TokenKind::kw_false))
        return std::make_unique<Literal>(tok);
    InvalidExprError();
    return nullptr;
}

std::unique_ptr<Expr> Parser::parseFunExpr(Token identifier) {
    consume(tok::TokenKind::L_PAREN);
    llvm::SmallVector<std::unique_ptr<Expr>, 256> params;
    // In fun expression, params can be identifiers or expressions
    while (!match(tok::TokenKind::R_PAREN) && !atEnd()) {
        std::unique_ptr<Expr> param = parseExpr();
        params.push_back(std::move(param));
        if (!match(tok::TokenKind::R_PAREN))
            consume(tok::TokenKind::COMMA);
    }

    consume(tok::TokenKind::R_PAREN);
    return std::make_unique<FunExpr>(identifier, params);
}

// STATEMENTS

//FIXME: For user defined types, add tok::TokenKind::IDENTIFIER
//       We need to also check if two identifiers are adjacent to distinguish
//       assignment and declarations
std::unique_ptr<Stmt> Parser::parseStmt() {
    if (match(tok::TokenKind::L_CURLY))
        return parseBlock();
    if (Tok.isOneOf(tok::TokenKind::kw_int, tok::TokenKind::kw_bool,
                    tok::TokenKind::kw_float, tok::TokenKind::kw_double,
                    tok::TokenKind::kw_string, tok::TokenKind::kw_void))
        return parseDeclareStmt();
    if (match(tok::TokenKind::kw_print))
        return parsePrint();
    if (match(tok::TokenKind::kw_read))
        return parseRead();
    if (match(tok::TokenKind::kw_return))
        return parseReturn();
    if(match(tok::TokenKind::kw_if))
        return parseIf();
    if(match(tok::TokenKind::kw_while))
        return parseWhile();
    if(match(tok::TokenKind::kw_until))
        return parseUntil();
    if(match(tok::TokenKind::kw_for))
        return parseFor();
    if(match(tok::TokenKind::kw_import))
        return parseImport();
    if(match(tok::TokenKind::kw_fun))
        return parseFunStmt();
    return parseExprStmt();
}

std::unique_ptr<Stmt> Parser::parseBlock() { 
    Token tok = Tok;
    consume(tok::TokenKind::L_CURLY);
    llvm::SmallVector<std::unique_ptr<Stmt>, 256> stmts;
    while (!match(tok::TokenKind::R_CURLY) && !match(tok::TokenKind::EOI)) {
        stmts.push_back(std::move(parseStmt()));
    }
    if (match(tok::TokenKind::EOI))
        unmatchedCharError(tok);
    consume(tok::TokenKind::R_CURLY);
    return std::make_unique<Block>(stmts);
}

std::unique_ptr<Stmt> Parser::parsePrint() { 
    std::unique_ptr<Expr> expr;
    consume(tok::TokenKind::kw_print);
    expr = parseExpression();
    panic();
    consume(tok::TokenKind::SEMI);
    return std::make_unique<Print>(std::move(expr));
}

std::unique_ptr<Stmt> Parser::parseRead() { 
    consume(tok::TokenKind::kw_read);
    expect(tok::TokenKind::IDENTIFIER);
    Token identifier = Tok;
    advance();
    panic();
    consume(tok::TokenKind::SEMI);
    return std::make_unique<Read>(identifier);
}

std::unique_ptr<Stmt> Parser::parseReturn() {
    std::unique_ptr<Expr> expr;
    llvm::SMLoc loc = Tok.getLocation();
    consume(tok::TokenKind::kw_return);
    expr = parseExpression();
    panic();
    consume(tok::TokenKind::SEMI);
    return std::make_unique<Return>(std::move(expr), loc);
}

std::unique_ptr<Stmt> Parser::parseIf() {
    std::unique_ptr<Expr> expr;
    std::unique_ptr<Stmt> ifStmt;
    std::unique_ptr<Stmt> elseStmt;

    llvm::SMLoc loc = Tok.getLocation();
    consume(tok::TokenKind::kw_if);
    expr = parseBoolExpr();
    ifStmt = parseStmt();
    if (match(tok::TokenKind::kw_else))
        elseStmt = parseStmt();
    
    return std::make_unique<If>(std::move(expr), std::move(ifStmt), std::move(elseStmt), loc);
}

std::unique_ptr<Stmt> Parser::parseWhile() { 
    std::unique_ptr<Expr> expr;
    std::unique_ptr<Stmt> stmt;

    llvm::SMLoc loc = Tok.getLocation();
    consume(tok::TokenKind::kw_while);
    expr = parseBoolExpr();
    stmt = parseStmt();

    return std::make_unique<While>(std::move(expr), std::move(stmt), loc);
}

std::unique_ptr<Stmt> Parser::parseUntil() { 
    std::unique_ptr<Expr> expr;
    std::unique_ptr<Stmt> stmt;

    llvm::SMLoc loc = Tok.getLocation();
    consume(tok::TokenKind::kw_until);
    expr = parseBoolExpr();
    stmt = parseStmt();

    return std::make_unique<Until>(std::move(expr), std::move(stmt), loc);
}

std::unique_ptr<Stmt> Parser::parseFor() { 
    std::unique_ptr<Stmt> decl;
    std::unique_ptr<Expr> cond;
    std::unique_ptr<Expr> change;
    std::unique_ptr<Stmt> stmt;

    llvm::SMLoc loc = Tok.getLocation();
    consume(tok::TokenKind::kw_for);
    consume(tok::TokenKind::L_PAREN);
    decl = parseDeclareStmt();
    cond = parseBoolExpr();
    consume(tok::TokenKind::SEMI);
    change = parseAssign();
    consume(tok::TokenKind::R_PAREN);
    stmt = parseStmt();

    return std::make_unique<For>(std::move(decl), std::move(cond), 
                                     std::move(change), std::move(stmt), loc);
}

std::unique_ptr<Stmt> Parser::parseFunStmt() { 
    consume(tok::TokenKind::kw_fun);
    expect(tok::TokenKind::IDENTIFIER);
    Token identifier = Tok;
    advance();
    consume(tok::TokenKind::L_PAREN);
    llvm::SmallVector<std::unique_ptr<Declare>, 256> params;
    while (!match(tok::TokenKind::R_PAREN) && !atEnd()) {
        std::unique_ptr<Declare> param = std::unique_ptr<Declare>(static_cast<Declare*>(parseDeclare().release()));
        params.push_back(std::move(param));
        if (!match(tok::TokenKind::R_PAREN))
            consume(tok::TokenKind::COMMA);
    }

    consume(tok::TokenKind::R_PAREN);
    consume(tok::TokenKind::ARROW);
    Token type;
    if (Tok.isOneOf(tok::TokenKind::kw_int, tok::TokenKind::kw_bool,
                    tok::TokenKind::kw_float, tok::TokenKind::kw_double,
                    tok::TokenKind::kw_string, tok::TokenKind::kw_void))
        type = Tok;
    else
        FunReturnError();

    advance();
    std::unique_ptr<Stmt> body = parseStmt();
    
    return std::make_unique<FunStmt>(identifier, params, type, std::move(body));
}

//TODO
std::unique_ptr<Stmt> Parser::parseClass() { 
    return nullptr;
}

std::unique_ptr<Stmt> Parser::parseImport() { 
    consume(tok::TokenKind::kw_import);
    expect(tok::TokenKind::STRING_LITERAL);
    Token string = Tok;
    advance();
    panic();
    consume(tok::TokenKind::SEMI);
    return std::make_unique<Import>(string);
}

std::unique_ptr<Stmt> Parser::parseDeclare() {
    Token type = Tok;
    advance();
    expect(tok::TokenKind::IDENTIFIER);
    std::unique_ptr<Expr> iden = std::make_unique<Variable>(Tok);
    advance();
    return std::make_unique<Declare>(type, std::move(iden));
}

std::unique_ptr<Stmt> Parser::parseDeclareStmt() {
    Token type = Tok;
    advance();
    expect(tok::TokenKind::IDENTIFIER);
    tok::TokenKind lookAhead = peek();
    std::unique_ptr<Expr> expr;
    if (lookAhead == tok::TokenKind::EQUAL ||
        lookAhead == tok::TokenKind::PLUSEQUAL ||
        lookAhead == tok::TokenKind::MINUSEQUAL)
        expr = parseAssign();
    else {
        expr = std::make_unique<Variable>(Tok);
        advance();
    }
    panic();
    consume(tok::TokenKind::SEMI);
    return std::make_unique<Declare>(type, std::move(expr));
}

std::unique_ptr<Stmt> Parser::parseExprStmt() {
    std::unique_ptr<Stmt> stmt = std::make_unique<ExprStmt>(std::move(parseExpression()));
    panic();
    consume(tok::TokenKind::SEMI);
    return stmt;
}
