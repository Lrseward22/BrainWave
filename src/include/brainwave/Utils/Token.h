#ifndef BRAINWAVE_UTILS_TOKEN_H
#define BRAINWAVE_UTILS_TOKEN_H

#include <brainwave/Utils/TokenKinds.h>
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include <cstddef>

using namespace brainwave;

class Token {
    friend class Lexer;

    const char *Ptr;
    size_t Length;
    tok::TokenKind Kind;

public:
    Token() {}
    Token(const char* data, size_t l, tok::TokenKind K)
        : Ptr(data), Length(l), Kind(K) { }
    tok::TokenKind getKind() const { return Kind; }
    size_t getLength() const { return Length; }

    llvm::SMLoc getLocation() const {
        return llvm::SMLoc::getFromPointer(Ptr);
    }

    llvm::StringRef getIdentifier() {
        assert(isOneOf(tok::IDENTIFIER, tok::kw_this) &&
                "Cannot get identifier of non-identifier");
        return llvm::StringRef(Ptr, Length);
    }

    llvm::StringRef getLiteralData() {
        if (is(tok::UNKNOWN)) return llvm::StringRef("Unknown Character");
        assert(isOneOf(tok::INTEGER_LITERAL,
                       tok::STRING_LITERAL,
                       tok::FLOAT_LITERAL,
                       tok::kw_true,
                       tok::kw_false,
                       tok::kw_null) &&
            "Cannot get literal data of non-literal");
        return llvm::StringRef(Ptr, Length);
    }

    llvm::StringRef getLexeme() {
        return llvm::StringRef(Ptr, Length);
    }

    bool is(tok::TokenKind K) const { return Kind == K; }

    bool isOneOf(tok::TokenKind K1, tok::TokenKind K2) const {
        return is(K1) || is(K2);
    }

    template <typename... Ts>
    bool isOneOf(tok::TokenKind K1, tok::TokenKind K2, Ts... Ks) const {
        return is(K1) || isOneOf(K2, Ks...);
    }
};

#endif
