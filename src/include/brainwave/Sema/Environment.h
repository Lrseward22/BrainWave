#ifndef BRAINWAVE_SEMA_ENVIRONMENT_H
#define BRAINWAVE_SEMA_ENVIRONMENT_H

#include <brainwave/Utils/Token.h>
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringMap.h"
#include <iostream>

enum class EnvKind {
    Base,
    If,
    Loop,
    Function,
    Class,
    Block
};

inline std::ostream& operator<<(std::ostream& os, EnvKind k) {
    switch (k) {
        case EnvKind::Base: return os << "Base";
        case EnvKind::If: return os << "If";
        case EnvKind::Loop: return os << "Loop";
        case EnvKind::Function: return os << "Function";
        case EnvKind::Class: return os << "Class";
        case EnvKind::Block: return os << "Block";
    }
    return os << "Unknown";
}

class Environment {
    Environment *parent;
    llvm::StringMap<llvm::StringRef> typeMap;
    llvm::StringMap<int> indexMap;
    int indexCount = 0;
    EnvKind kind;

    public:
    Environment(EnvKind kind, Environment *parent) 
        : parent(parent), kind(kind) {}

    bool define(llvm::StringRef name, llvm::StringRef type);
    llvm::StringRef get(Token name);
    int getIndex(llvm::StringRef name);
    int getIndex(Token name);
    int numVars() { return indexCount; }
    EnvKind getKind() { return kind; }
    Environment* getParent() { return parent; }
};

#endif
