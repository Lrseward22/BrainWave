#ifndef BRAINWAVE_UTILS_TYPE_H
#define BRAINWAVE_UTILS_TYPE_H

#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringMap.h"
#include <string>

namespace brainwave {

    namespace Ty {
        enum TypeKind: unsigned short {
            Void,
            Numeric,
            Bool,
            String,
            Array,
            Math,
            IO,
            UserDefined
        };

        extern llvm::StringMap<TypeKind> ValidTypes;

        void initializeBuiltInTypes();

        TypeKind getTypeKind(llvm::StringRef name);
        void declareType(llvm::StringRef name, TypeKind type);
        bool declareType(llvm::StringRef name);
        bool isDefined(llvm::StringRef name);

        class Type {
            std::string name;
            TypeKind Kind;

        public:
            Type() : name("void"), Kind(TypeKind::Void) { }
            Type(const Type& type) : name(type.get()), Kind(type.getKind()) { }
            Type(llvm::StringRef name) : name(name.str()) {
                Kind = getTypeKind(this->name);
            }
            Type(const char* name) : Type(llvm::StringRef(name)) { }
            Type(const std::string& name) : name(name) {
                Kind = getTypeKind(this->name);
            }

            bool is(TypeKind K) const;
            bool isNumeric() const;
            bool isUserDefined() const;
            llvm::StringRef get() const;
            TypeKind getKind() const;
        };

        bool equals(const Type& t1, const Type& t2);
        bool resolvable(const Type& t1, const Type& t2);
        bool resolve(const Type& t1, const Type& t2);
    }
}

#endif
