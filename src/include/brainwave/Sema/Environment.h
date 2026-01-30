#ifndef BRAINWAVE_SEMA_ENVIRONMENT_H
#define BRAINWAVE_SEMA_ENVIRONMENT_H

#include <brainwave/Utils/Token.h>
#include <brainwave/Utils/Type.h>
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/Instructions.h"
#include <iostream>
#include <memory>

class FunStmt;
class ClassStmt;
class ClassInfo;
class Environment;

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

class ClassInfo {
    friend class Environment;
    Environment* env;
    std::unique_ptr<ClassStmt> def;

    public:
    ClassInfo() : env(nullptr), def(nullptr) { }
    ClassInfo(Environment* env) : env(env), def(nullptr) { }
    ClassInfo(Environment* env, std::unique_ptr<ClassStmt> def) : env(env), def(std::move(def)) { }
};

class Environment {
    Environment *parent;
    llvm::StringMap<Ty::Type> typeMap;
    llvm::StringMap<llvm::AllocaInst*> allocations;
    llvm::StringMap<llvm::SmallVector<std::unique_ptr<FunStmt>, 256>> funcMap;
    llvm::StringMap<ClassInfo> classMap;
    llvm::StringMap<int> indexMap;
    int indexCount = 0;
    EnvKind kind;

    public:
    Environment(EnvKind kind, Environment *parent) 
        : parent(parent), kind(kind) {}

    bool defineVar(llvm::StringRef name, Ty::Type type);
    Ty::Type* getVar(Token name);
    bool declareAlloca(llvm::StringRef name, llvm::AllocaInst* alloca);
    llvm::AllocaInst* getAlloca(llvm::StringRef name);
    int getIndex(llvm::StringRef name);
    int getIndex(Token name);
    int numVars() { return indexCount; }
    void defineFunc(llvm::StringRef name);
    void attachFunc(llvm::StringRef name, std::unique_ptr<FunStmt> FuncAST);
    llvm::SmallVector<std::unique_ptr<FunStmt>, 256>* getFunc(llvm::StringRef name);
    const llvm::StringMap<llvm::SmallVector<std::unique_ptr<FunStmt>, 256>>& getFuncs() { return funcMap; }
    bool defineClass(llvm::StringRef name, Environment* ClassEnv);
    bool defineClass(llvm::StringRef name);
    void attachClass(llvm::StringRef name, std::unique_ptr<ClassStmt> stmt);
    Environment* getClass(llvm::StringRef name);
    bool hasMember(llvm::StringRef name);
    bool isLocal(llvm::StringRef name);
    bool isGlobal(llvm::StringRef name);
//    llvm::SmallVector<FunStmt*, 256> getAllFuncs() { 
//        llvm::SmallVector<FunStmt*, 256> funcs;
//        for (auto& entry : funcMap)
//            funcs.push_back(entry.getValue().get());
//        return funcs;
//    }
    EnvKind getKind() { return kind; }
    Environment* getParent() { return parent; }
};

#endif
