#include <brainwave/Sema/Environment.h>

bool Environment::define(llvm::StringRef name, llvm::StringRef type) {
    // Add error for redeclaration of variables (BAD)
    if (typeMap.count(name))
        return true;
    typeMap[name] = type;
    indexMap[name] = indexCount++;
    return false;
}

llvm::StringRef Environment::get(Token name) {
    if (typeMap.count(name.getLexeme()))
        return typeMap[name.getLexeme()];
    else if (parent != nullptr)
        return parent->get(name);
    // Error: Variable isn't defined
    return "";
}

int Environment::getIndex(llvm::StringRef name) {
    return indexMap[name];
}

int Environment::getIndex(Token name) {
    return getIndex(name.getLexeme());
}
