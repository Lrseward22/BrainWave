#include <brainwave/Lexer/Lexer.h>
#include <iostream>

namespace charinfo {
    LLVM_READNONE inline bool isHorizontalWhitespace(char c) {
        return c == ' ' || c == '\t' || c == '\f' || c == '\v';
    }

    LLVM_READNONE inline bool isVerticalWhitespace(char c) {
        return c == '\r' || c == '\n';
    }

    LLVM_READNONE inline bool isWhitespace(char c) {
        return isHorizontalWhitespace(c) || isVerticalWhitespace(c);
    }

    LLVM_READNONE inline bool isDigit(char c) {
        return c >= '0' && c <= '9';
    }

    LLVM_READNONE inline bool isLetter(char c) {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
    }

    LLVM_READNONE inline bool isAlphaNumeric(char c) {
        return isDigit(c) || isLetter(c);
    }
}

void KeywordFilter::addKeyword(llvm::StringRef Keyword, tok::TokenKind TokenCode) {
    HashTable.insert(std::make_pair(Keyword, TokenCode));
}

void KeywordFilter::addKeywords() {
    #define KEYWORD(NAME, FLAGS) addKeyword(llvm::StringRef(#NAME), tok::kw_##NAME);
    #include <brainwave/Utils/TokenKinds.def>
}

void Lexer::next(Token &token) {
    while (*BufferPtr && charinfo::isWhitespace(*BufferPtr)) {
        BufferPtr++;
    }
    if (!*BufferPtr) {
        token.Kind = tok::EOI;
        return;
    }
    if (charinfo::isLetter(*BufferPtr)) {
        const char *end = BufferPtr + 1;
        while (charinfo::isAlphaNumeric(*end) || *end == '_')
            end++;
        llvm::StringRef Name(BufferPtr, end - BufferPtr);
        tok::TokenKind kind = KF.getKeyword(Name, tok::IDENTIFIER);
        formToken(token, end, kind);
        return;
    } else if (charinfo::isDigit(*BufferPtr)) {
        const char *end = BufferPtr + 1;
        bool hasRadix = false;
        while (charinfo::isDigit(*end) || *end == '.') {
            if (*end == '.' && hasRadix==false)
                hasRadix = true;
            else if (*end == '.' && hasRadix==true)
                Diag.report(getLoc(), diag::err_malformed_float);
            end++;
        }
        if (hasRadix)
            formToken(token, end, tok::FLOAT_LITERAL);
        else
            formToken(token, end, tok::INTEGER_LITERAL);
        return;
    } else if (*BufferPtr == '"') {
        const char *end = ++BufferPtr;
        while (*end != '"' && !charinfo::isVerticalWhitespace(*end)) {
            end++;
            if (*end == '\0')
                break;
        }
        if (charinfo::isVerticalWhitespace(*end))
            Diag.report(getLoc(), diag::err_unterminated_char, *(BufferPtr-1));
        formToken(token, end, tok::STRING_LITERAL);
        BufferPtr++;
    } else {
        switch (*BufferPtr) {
            case '+': 
                if (*(BufferPtr + 1) == '+')
                    formToken(token, BufferPtr + 2, tok::PLUSPLUS);
                else if (*(BufferPtr + 1) == '=')
                    formToken(token, BufferPtr + 2, tok::PLUSEQUAL);
                else
                    formToken(token, BufferPtr + 1, tok::PLUS);
                break;
            case '-': 
                if (*(BufferPtr + 1) == '-')
                    formToken(token, BufferPtr + 2, tok::MINUSMINUS);
                else if (*(BufferPtr + 1) == '>'    )
                    formToken(token, BufferPtr + 2, tok::ARROW);
                else if (*(BufferPtr + 1) == '=')
                    formToken(token, BufferPtr + 2, tok::MINUSEQUAL);
                else
                    formToken(token, BufferPtr + 1, tok::MINUS);
                break;
            case '*': 
                formToken(token, BufferPtr + 1, tok::STAR);
                break;
            case '/': 
                formToken(token, BufferPtr + 1, tok::SLASH);
                break;
            case '^': 
                formToken(token, BufferPtr + 1, tok::CARET);
                break;
            case '%': 
                formToken(token, BufferPtr + 1, tok::PERCENT);
                break;
            case ',': 
                formToken(token, BufferPtr + 1, tok::COMMA);
                break;
            case '.': 
                formToken(token, BufferPtr + 1, tok::PERIOD);
                break;
            case ';': 
                formToken(token, BufferPtr + 1, tok::SEMI);
                break;
            case ':': 
                formToken(token, BufferPtr + 1, tok::COLON);
                break;
            case '=': 
                if (*(BufferPtr + 1) == '=')
                    formToken(token, BufferPtr + 2, tok::EQ);
                else
                    formToken(token, BufferPtr + 1, tok::EQUAL);
                break;
            case '!': 
                if (*(BufferPtr + 1) == '=')
                    formToken(token, BufferPtr + 2, tok::NEQ);
                else
                    formToken(token, BufferPtr + 1, tok::BANG);
                break;
            case '<': 
                if (*(BufferPtr + 1) == '=')
                    formToken(token, BufferPtr + 2, tok::LEQ);
                else
                    formToken(token, BufferPtr + 1, tok::LESS);
                break;
            case '>': 
                if (*(BufferPtr + 1) == '=')
                    formToken(token, BufferPtr + 2, tok::GEQ);
                else
                    formToken(token, BufferPtr + 1, tok::GREATER);
                break;
            case '(':
                formToken(token, BufferPtr + 1, tok::L_PAREN);
                break;
            case ')':
                formToken(token, BufferPtr + 1, tok::R_PAREN);
                break;
            case '[':
                formToken(token, BufferPtr + 1, tok::L_BRACK);
                break;
            case ']':
                formToken(token, BufferPtr + 1, tok::R_BRACK);
                break;
            case '{':
                formToken(token, BufferPtr + 1, tok::L_CURLY);
                break;
            case '}':
                formToken(token, BufferPtr + 1, tok::R_CURLY);
                break;
            case '&':
                if (*(BufferPtr + 1) == '&') {
                    formToken(token, BufferPtr + 2, tok::AND);
                    break;
                }
            case '|':
                if (*(BufferPtr + 1) == '|') {
                    formToken(token, BufferPtr + 2, tok::OR);
                    break;
                }
            default:
                Diag.report(getLoc(), diag::err_illegal_char);
                formToken(token, BufferPtr + 1, tok::UNKNOWN);
        }
        return;
    }
}

tok::TokenKind Lexer::peek() {
    const char *buffPtr = BufferPtr;
    Token tok{};
    next(tok);
    BufferPtr = buffPtr;
    return tok.getKind();
}

void Lexer::formToken(Token &Tok, const char *TokEnd, tok::TokenKind Kind) {
    Tok.Kind = Kind;
    Tok.Ptr = BufferPtr;
    Tok.Length = TokEnd - BufferPtr;
    BufferPtr = TokEnd;
}
