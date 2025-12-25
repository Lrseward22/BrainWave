#ifndef BRAINWAVE_LEXER_TOKEN_A
#define BRAINWAVE_LEXER_TOKEN_A

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
    tok::TokenKind getKind() const { return Kind; }
    size_t getLength() const { return Length; }

    llvm::SMLoc getLocation() const {
        return llvm::SMLoc::getFromPointer(Ptr);
    }

    llvm::StringRef getIdentifier() {
        assert(is(tok::IDENTIFIER) &&
                "Cannot get identifier of non-identifier");
        return llvm::StringRef(Ptr, Length);
    }

    llvm::StringRef getLiteralData() {
        if (is(tok::UNKNOWN)) return llvm::StringRef("Unknown Character");
        assert(isOneOf(tok::INTEGER_LITERAL,
                       tok::STRING_LITERAL,
                       tok::FLOAT_LITERAL,
                       tok::kw_true,
                       tok::kw_false) &&
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
