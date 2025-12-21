#ifndef BRAINWAVE_SEMA_ENVIRONMENT_H
#define BRAINWAVE_SEMA_ENVIRONMENT_H

#include <brainwave/Utils/Token.h>
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringMap.h"

enum class EnvKind {
    Base,
    If,
    Loop,
    Function,
    Class,
    Block
};

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
