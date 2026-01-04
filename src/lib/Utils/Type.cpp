#include <brainwave/Utils/Type.h>
#include <cstddef>

using namespace brainwave;

llvm::StringMap<Ty::TypeKind> Ty::ValidTypes;

void Ty::initializeBuiltInTypes() {
    ValidTypes["void"] = TypeKind::Void;
    ValidTypes["bool"] = TypeKind::Bool;
    ValidTypes["int"] = TypeKind::Numeric;
    ValidTypes["float"] = TypeKind::Numeric;
    ValidTypes["double"] = TypeKind::Numeric;
    ValidTypes["string"] = TypeKind::String;
    ValidTypes["Array"] = TypeKind::Array;
    ValidTypes["Math"] = TypeKind::Math;
    ValidTypes["IO"] = TypeKind::IO;
}

Ty::TypeKind Ty::getTypeKind(llvm::StringRef name) {
    return ValidTypes[name];
}
void Ty::declareType(llvm::StringRef name, TypeKind type) {
    ValidTypes[name] = type;
}
bool Ty::declareType(llvm::StringRef name) {
    if (ValidTypes.count(name))
        return true;
    declareType(name, UserDefined);
    return false;
}
bool Ty::isDefined(llvm::StringRef name) {
    if (ValidTypes.count(name))
        return true;
    return false;
}

bool Ty::Type::is(TypeKind K) const {
    return Kind == K;
}
bool Ty::Type::isNumeric() const { 
    return is(TypeKind::Numeric); 
}
bool Ty::Type::isUserDefined() const {
    return is(TypeKind::UserDefined);
}
llvm::StringRef Ty::Type::get() const {
    return name;
}
Ty::TypeKind Ty::Type::getKind() const {
    return Kind;
}

bool Ty::equals(const Type& t1, const Type& t2) {
    return (t1.get() == t2.get());
}
bool Ty::resolvable(const Type& t1, const Type& t2) {
    return ((t1.isNumeric() && t2.isNumeric())
            || equals(t1, t2));
}
bool Ty::resolve(const Type& t1, const Type& t2) {
    assert((t1.isNumeric() && t2.isNumeric()) && 
            "Cannot resolve types of non-resolvables");
    llvm::StringRef type1 = t1.get();
    llvm::StringRef type2 = t2.get();
    if ((type1 == "int" && (type2 == "float" || type2 == "double")) ||
        (type1 == "float" && type2 == "double"))
        return true;
    else return false;
}
