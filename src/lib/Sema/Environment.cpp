#include <brainwave/Sema/Environment.h>
#include <brainwave/Parser/AST.h>

bool Environment::defineVar(llvm::StringRef name, Ty::Type type) {
    if (typeMap.count(name))
        return true;
    typeMap[name] = type;
    indexMap[name] = indexCount++;
    return false;
}

Ty::Type* Environment::getVar(Token name) {
    if (typeMap.count(name.getLexeme()))
        return &typeMap[name.getLexeme()];
    else if (parent != nullptr)
        return parent->getVar(name);
    return nullptr;
}

bool Environment::declareAlloca(llvm::StringRef name, llvm::AllocaInst* alloca) {
    if (allocations.count(name))
        return true;
    allocations[name] = alloca;
    return false;
}

llvm::AllocaInst* Environment::getAlloca(llvm::StringRef name) {
    if (allocations.count(name))
        return allocations[name];
    else if (parent != nullptr)
        return parent->getAlloca(name);
    return nullptr;
}

bool Environment::defineFunc(llvm::StringRef name, std::unique_ptr<FunStmt> funcAST) {
    if (funcMap.count(name))
        return true;
    funcMap[name] = std::move(funcAST);
    return false;
}

bool Environment::defineFunc(llvm::StringRef name) {
    if (funcMap.count(name))
        return true;
    funcMap[name] = nullptr;
    return false;
}

FunStmt* Environment::getFunc(llvm::StringRef name) {
    if (funcMap.count(name) && funcMap[name]) 
        return funcMap[name].get();
    else if (parent != nullptr)
        return parent->getFunc(name);
    return nullptr;
}

void Environment::attachFunc(llvm::StringRef name, std::unique_ptr<FunStmt> FuncAST) {
    funcMap[name] = std::move(FuncAST);
}

bool Environment::defineClass(llvm::StringRef name, Environment* ClassEnv) {
    if (classMap.count(name))
        return true;
    classMap[name] = ClassInfo(ClassEnv);
    return false;
}

bool Environment::defineClass(llvm::StringRef name) {
    if (classMap.count(name))
        return true;
    classMap[name] = ClassInfo();
    return false;
}

void Environment::attachClass(llvm::StringRef name, std::unique_ptr<ClassStmt> stmt) {
    classMap[name].def = std::move(stmt);
}

Environment* Environment::getClass(llvm::StringRef name) {
    if (classMap.count(name) && classMap[name].env) 
        return classMap[name].env;
    else if (parent != nullptr)
        return parent->getClass(name);
    return nullptr;
}

bool Environment::hasMember(llvm::StringRef name) {
    if (kind != EnvKind::Class && kind != EnvKind::Base)
        return parent->hasMember(name);
    if (kind == EnvKind::Class)
        return (typeMap.count(name) || funcMap.count(name));
    else return false;
}

bool Environment::isLocal(llvm::StringRef name) {
    if (kind != EnvKind::Class && typeMap.count(name))
        return true;
    else if (kind != EnvKind::Class && !typeMap.count(name)) {
        if (parent) {
            return parent->isLocal(name);
        } else return false;
    } else /* Environment must be a Class Environment */
        return false;
}

bool Environment::isGlobal(llvm::StringRef name) {
    if (kind == EnvKind::Base)
        return typeMap.count(name);
    if (parent)
        return parent->isGlobal(name);
    return false;
}

int Environment::getIndex(llvm::StringRef name) {
    return indexMap[name];
}

int Environment::getIndex(Token name) {
    return getIndex(name.getLexeme());
}
