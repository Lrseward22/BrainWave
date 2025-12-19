#include <brainwave/Utils/TokenKinds.h>
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <iostream>

using namespace brainwave;

static const char * const TokNames[] = {
#define TOK(ID) #ID,
#define KEYWORD(ID, FLAG) #ID,
#include <brainwave/Utils/TokenKinds.def>
#undef KEYWORD
#undef TOK
    nullptr
};

const char *tok::getTokenName(TokenKind Kind) {
    return TokNames[Kind];
}

const char *tok::getPunctuatorSpelling(TokenKind Kind) {
    switch (Kind) {
#define PUNCTUATOR(ID, SP) case ID: return SP;
#include <brainwave/Utils/TokenKinds.def>
#undef PUNCTUATOR
        default: break;
    }
    return nullptr;
}

const char * tok::getKeywordSpelling(TokenKind Kind) {
    switch (Kind) {
#define KEYWORD(ID, FLAG) case kw_ ## ID: return #ID;
#include <brainwave/Utils/TokenKinds.def>
#undef KEYWORD
        default: break;
    }
    return nullptr;
}

#undef TOK
#undef IDENTIFIER
#undef LITERAL
#undef PUNCTUATOR
#undef KEYWORD


#define TOK(ID)             TC_Other,
#define IDENTIFIER(ID)      TC_Identifier,
#define LITERAL(ID)         TC_Literal,
#define PUNCTUATOR(ID, SP)  TC_Punctuator,
#define KEYWORD(ID, FLAG)   TC_Keyword,

const tok::TokenCategory tok::TokenCategories[] = {
    #include <brainwave/Utils/TokenKinds.def>
    #undef TOK
    #undef IDENTIFIER
    #undef LITERAL
    #undef PUNCTUATOR
    #undef KEYWORD
    tok::TC_Other
};

bool tok::isIdentifier(TokenKind Kind) {
    return tok::TokenCategories[Kind] == TC_Identifier;
}

bool tok::isLiteral(TokenKind Kind) {
    return tok::TokenCategories[Kind] == TC_Literal;
}

bool tok::isPunctuator(TokenKind Kind) {
    return tok::TokenCategories[Kind] == TC_Punctuator;
}

bool tok::isKeyword(TokenKind Kind) {
    return tok::TokenCategories[Kind] == TC_Keyword;
}

std::string tok::formatTokenKind(TokenKind Kind) {
    std::string buffer;
    if (tok::isIdentifier(Kind))
        buffer = "identifier";
    else if (tok::isLiteral(Kind)) {
        buffer = std::string(tok::getTokenName(Kind));
        for (char& c : buffer) {
            if (c == '_') c = ' ';
            else c = std::tolower(c);
        }
    } else if (tok::isPunctuator(Kind))
        buffer = "\'" + std::string(tok::getPunctuatorSpelling(Kind)) + "\'";
    else if (tok::isKeyword(Kind))
        buffer = "key word \'" + std::string(tok::getKeywordSpelling(Kind)) + "\'";
    else
        buffer = "end of input or unformattable token";
    return buffer;
}
