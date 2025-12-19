#include <brainwave/Sema/Environment.h>

void Environment::define(llvm::StringRef name, llvm::StringRef type) {
    // Add error for redeclaration of variables (BAD)
    typeMap[name] = type;
    indexMap[name] = indexCount++;
}

llvm::StringRef Environment::get(Token name) {
    if (typeMap.count(name.getLexeme()))
        return typeMap[name.getLexeme()];
    else if (parent != nullptr)
        return parent->get(name);
    //Add error: Variable isn't defined
    return "";
}

int Environment::getIndex(llvm::StringRef name) {
    return indexMap[name];
}

int Environment::getIndex(Token name) {
    return getIndex(name.getLexeme());
}
