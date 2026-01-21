#ifndef BRAINWAVE_UTILS_TYPE_H
#define BRAINWAVE_UTILS_TYPE_H

#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/Hashing.h"
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
        bool isCastable(llvm::StringRef name);

        class Type {
            std::string name;
            TypeKind Kind;

        public:
            Type() : name("void"), Kind(TypeKind::Void) { }
            Type(const Type& type) : name(type.name), Kind(type.Kind) { }
            Type(std::string name, TypeKind K) : name(name), Kind(K) { }
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
            bool isCastable() const;
            llvm::StringRef get() const;
            const std::string& str() const { return name; }
            TypeKind getKind() const;
            void setKind(TypeKind K) { Kind = K; };

            bool operator==(const Type& Other) const {
                return Kind == Other.Kind && name == Other.name;
            }
            bool operator!=(const Type& Other) const {
                return !(*this == Other);
            }
        };

        bool equals(const Type& t1, const Type& t2);
        bool resolvable(const Type& t1, const Type& t2);
        bool resolve(const Type& t1, const Type& t2);
    }
}

namespace llvm {
    template<>
    struct DenseMapInfo<brainwave::Ty::Type> {
        using TyKind = brainwave::Ty::TypeKind;
        using TyType = brainwave::Ty::Type;

        static inline TyType getEmptyKey() {
            return TyType("<empty>", TyKind::Void);
        }

        static inline TyType getTombstoneKey() {
            return TyType("<tombstone>", TyKind::Void);
        }

        static unsigned getHashValue(const TyType& T) {
            return hash_combine(
                    static_cast<unsigned>(T.getKind()),
                    hash_value(T.get()));
        }

        static bool isEqual(const TyType& LHS, const TyType& RHS) {
            return LHS.getKind() == RHS.getKind() &&
                   LHS.str() == RHS.str();
        }
    };
}

#endif
