#include <brainwave/Sema/Environment.h>
#include <brainwave/Parser/AST.h>

bool Environment::defineVar(llvm::StringRef name, llvm::StringRef type) {
    if (typeMap.count(name))
        return true;
    typeMap[name] = type;
    indexMap[name] = indexCount++;
    return false;
}

llvm::StringRef Environment::getVar(Token name) {
    if (typeMap.count(name.getLexeme()))
        return typeMap[name.getLexeme()];
    else if (parent != nullptr)
        return parent->getVar(name);
    return "";
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

void Environment::attachFunc(llvm::StringRef name, std::unique_ptr<FunStmt> FuncAST) {
    funcMap[name] = std::move(FuncAST);
}

FunStmt* Environment::getFunc(llvm::StringRef name) {
    if (funcMap.count(name) && funcMap[name]) 
        return funcMap[name].get();
    else if (parent != nullptr)
        return parent->getFunc(name);
    return nullptr;
}

int Environment::getIndex(llvm::StringRef name) {
    return indexMap[name];
}

int Environment::getIndex(Token name) {
    return getIndex(name.getLexeme());
}
