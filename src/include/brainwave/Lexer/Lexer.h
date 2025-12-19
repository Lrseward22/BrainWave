#ifndef BRAINWAVE_LEXER_LEXER_H
#define BRAINWAVE_LEXER_LEXER_H

#include <brainwave/Utils/Token.h>
#include <brainwave/Utils/Diagnostics.h>
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/MemoryBuffer.h"
//#include <iostream>

class KeywordFilter {
    llvm::StringMap<tok::TokenKind> HashTable;
    void addKeyword(llvm::StringRef Keyword, tok::TokenKind TokenCode);

    public:
    void addKeywords();

    tok::TokenKind getKeyword(llvm::StringRef Name, tok::TokenKind DefaultTokenCode = tok::UNKNOWN) {
        auto Result = HashTable.find(Name);
        if (Result != HashTable.end())
            return Result->second;
        return DefaultTokenCode;
    }
};

class Lexer {
    const char *BufferPtr;
    const llvm::SourceMgr &SrcMgr;
    brainwave::DiagnosticsEngine &Diag;
    KeywordFilter KF;

public:
    Lexer(const llvm::SourceMgr &SrcMgr, brainwave::DiagnosticsEngine &Diag) :
    SrcMgr(SrcMgr), Diag(Diag) {
        const llvm::MemoryBuffer *Buffer = SrcMgr.getMemoryBuffer(SrcMgr.getMainFileID());
        const char *BufferStart = Buffer->getBufferStart();
        BufferPtr = BufferStart;
        KF.addKeywords();
    }

    brainwave::DiagnosticsEngine &getDiagnostics() const {
        return Diag;
    }

    void next (Token &token);

    tok::TokenKind peek();

private:
    void formToken(Token &Result, const char *TokEnd, tok::TokenKind Kind);
    llvm::SMLoc getLoc() { return llvm::SMLoc::getFromPointer(BufferPtr); }
};

#endif

