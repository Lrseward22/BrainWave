#include <brainwave/Sema/Environment.h>

bool Environment::defineVar(llvm::StringRef name, llvm::StringRef type) {
    // Add error for redeclaration of variables (BAD)
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
    // Error: Variable isn't defined
    return "";
}

bool Environment::defineFunc(llvm::StringRef name, AST* funcAST) {
    // Add error for redeclaration of Function (BAD)
    if (typeMap.count(name))
        return true;
    funcMap[name] = funcAST;
    return false;
}

AST* Environment::getFunc(llvm::StringRef name) {
    if (funcMap.count(name))
        return funcMap[name];
    else if (parent != nullptr)
        return parent->getFunc(name);
    // Error: Variable isn't defined
    return nullptr;
}

int Environment::getIndex(llvm::StringRef name) {
    return indexMap[name];
}

int Environment::getIndex(Token name) {
    return getIndex(name.getLexeme());
}
