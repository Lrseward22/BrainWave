#include <brainwave/Generator/CodeGen.h>
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Host.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <iostream>

using namespace llvm;

/** TODO: 
 */

namespace {
class IRVisitor : public ASTVisitor {
    Module* M;
    IRBuilder<> Builder;
    DenseMap<Ty::Type, Type*> TypeMap;
    //Type* VoidTy;
    //Type* mapType(Ty::Type("bool"));
    //Type* mapType(Ty::Type("int"));
    //Type* mapType(Ty::Type("float"));
    //Type* mapType(Ty::Type("double"));
    PointerType *PtrTy;
    //Value* PrintStr;
    //Value* ReadStr;
    Value* V;
    Function* Fn;
    BasicBlock* Curr;

    Environment* env;
    BasicBlock* ContBB;
    BasicBlock* BreakBB;
    bool AddressMode = false;

    BasicBlock* createBasicBlock(const Twine& name,
                      BasicBlock* InsertBefore = nullptr) {
        return BasicBlock::Create(M->getContext(), name, Fn, InsertBefore);
    }

    void setCurr(BasicBlock* BB) {
        Curr = BB;
        Builder.SetInsertPoint(Curr);
    }

    Type* mapType(Ty::Type T) {
        if (Type* Ty = TypeMap.lookup(T))
            return Ty;

        llvm::errs() << "No LLVM type mapping for Ty::Type: "
                     << T.str() << "(kind " << (unsigned)T.getKind() << ")\n";
        llvm::report_fatal_error("Misssing LLVM type mapping");
    }

public:
    IRVisitor(Module* M, Environment* env) : M(M), env(env), Builder(M->getContext()) {
        TypeMap[Ty::Type()] = Type::getVoidTy(M->getContext());
        TypeMap[Ty::Type("bool")] = Type::getInt1Ty(M->getContext());
        TypeMap[Ty::Type("int")] = Type::getInt32Ty(M->getContext());
        TypeMap[Ty::Type("float")] = Type::getFloatTy(M->getContext());
        TypeMap[Ty::Type("double")] = Type::getDoubleTy(M->getContext());
        PtrTy = Type::getInt8PtrTy(M->getContext());

        //PrintFTy = FunctionType::get(Builder.getmapType(Ty::Type("int"))(), Builder.getInt8PtrTy(), true);
        //PrintF = M->getOrInsertFunction("printf", PrintFTy);
        //ScanFTy = FunctionType::get(Builder.getmapType(Ty::Type("int"))(), Builder.getInt8PtrTy(), true);
        //ScanF = M->getOrInsertFunction("scanf", ScanFTy);
    }

    void createMain() {
        Type* Int32 = mapType(Ty::Type("int"));
        FunctionType* MainFty = FunctionType::get(
                Int32, {Int32, PtrTy}, false);
        Fn = Function::Create(
                MainFty, GlobalValue::ExternalLinkage, "main", M);
        BasicBlock* MainBB = createBasicBlock("entry");
        setCurr(MainBB);
    }

    void finishMain() {
        Builder.CreateRet(ConstantInt::get(mapType(Ty::Type("int")), 1, true));
    }

    void run(AST* Tree) {
        Tree->print();
        std::cout << "\n";
        Tree->accept(*this);
        //Builder.CreateCall(PrintF, {PrintStr, V});
    }

    // Expression ASTs
    virtual void visit(BinaryOp &expr) override {
        Value* left;
        Value* right;

        if (expr.getOp().is(tok::TokenKind::PERIOD)) {
            Ty::Type classType = expr.getLeft()->getType();
            Environment* classEnv = env->getClass(classType.get());
            Variable* fieldVar = dyn_cast<Variable>(expr.getRight());
            Token field = fieldVar->getIdentifier();
            int varIndex = classEnv->getIndex(field);

            bool prevMode = AddressMode;
            AddressMode = true;
            expr.getLeft()->accept(*this);
            AddressMode = prevMode;

            Value* classPtr = V;
            Value* fieldPtr = Builder.CreateStructGEP(
                    mapType(classType), classPtr, varIndex, field.getIdentifier());
            if (AddressMode)
                V = fieldPtr;
            else
                V = Builder.CreateLoad(mapType(expr.getType()), fieldPtr, field.getIdentifier());
            return;
        }

        expr.getLeft()->accept(*this);
        left = V;
        expr.getRight()->accept(*this);
        right = V;
        Ty::Type Type = expr.getType();

        switch (expr.getOp().getKind()) {
            case tok::TokenKind::PLUS:
                if (Type.get() == "string");
                // FIXME: String Concatonation
                else
                    V = Builder.CreateAdd(left, right);
                break;
            case tok::TokenKind::MINUS:
                V = Builder.CreateSub(left, right);
                break;
            case tok::TokenKind::STAR:
                V = Builder.CreateMul(left, right);
                break;
            case tok::TokenKind::SLASH:
                if (Type.get() == "int") V = Builder.CreateSDiv(left, right);
                else V = Builder.CreateFDiv(left, right);
                break;
            case tok::TokenKind::CARET:
                if (Type.get() == "int") {
                    // FIXME: No build in power for ints. Runtime library it probably
                } else if (Type.get() == "float") {
                    Function* PowFn = Intrinsic::getDeclaration(
                            M, Intrinsic::pow, {mapType(Ty::Type("float"))});
                    V = Builder.CreateCall(PowFn, {left, right});
                } else if (Type.get() == "double") {
                    Function* PowFn = Intrinsic::getDeclaration(
                            M, Intrinsic::pow, {mapType(Ty::Type("double"))});
                    V = Builder.CreateCall(PowFn, {left, right});
                }
                break;
            case tok::TokenKind::PERCENT:
                V = Builder.CreateSRem(left, right);
                break;
            case tok::TokenKind::EQ:
                if (Type.get() == "int" || Type.get() == "bool")
                    V = Builder.CreateICmpEQ(left, right);
                else if (Type.get() == "float" || Type.get() == "double")
                    V = Builder.CreateFCmpOEQ(left, right);
                else if (Type.is(Ty::TypeKind::String))
                    // FIXME: Runtime library for this
                    ;
                break;
            case tok::TokenKind::NEQ:
                if (Type.get() == "int" || Type.get() == "bool")
                    V = Builder.CreateICmpNE(left, right);
                else if (Type.get() == "float" || Type.get() == "double")
                    V = Builder.CreateFCmpONE(left, right);
                else if (Type.is(Ty::TypeKind::String))
                    // FIXME: Runtime library for this
                    ;
                break;
            case tok::TokenKind::LESS:
                if (Type.get() == "int" || Type.get() == "bool")
                    V = Builder.CreateICmpSLT(left, right);
                else if (Type.get() == "float" || Type.get() == "double")
                    V = Builder.CreateFCmpOLT(left, right);
                else if (Type.is(Ty::TypeKind::String))
                    // FIXME: Runtime library for this
                    ;
                break;
            case tok::TokenKind::LEQ:
                if (Type.get() == "int" || Type.get() == "bool")
                    V = Builder.CreateICmpSLE(left, right);
                else if (Type.get() == "float" || Type.get() == "double")
                    V = Builder.CreateFCmpOLE(left, right);
                else if (Type.is(Ty::TypeKind::String))
                    // FIXME: Runtime library for this
                    ;
                break;
            case tok::TokenKind::GREATER:
                if (Type.get() == "int" || Type.get() == "bool")
                    V = Builder.CreateICmpSGT(left, right);
                else if (Type.get() == "float" || Type.get() == "double")
                    V = Builder.CreateFCmpOGT(left, right);
                else if (Type.is(Ty::TypeKind::String))
                    // FIXME: Runtime library for this
                    ;
                break;
            case tok::TokenKind::GEQ:
                if (Type.get() == "int" || Type.get() == "bool")
                    V = Builder.CreateICmpSGE(left, right);
                else if (Type.get() == "float" || Type.get() == "double")
                    V = Builder.CreateFCmpOGE(left, right);
                else if (Type.is(Ty::TypeKind::String))
                    // FIXME: Runtime library for this
                    ;
                break;
        }
    };
    virtual void visit(UnaryOp &expr) override {
        expr.getExpr()->accept(*this);
        Value* e = V;
        Ty::Type Type = expr.getType();
        Constant* One;
        switch (expr.getOp().getKind()) {
            case tok::TokenKind::BANG:
                V = Builder.CreateNot(e);
                break;
            case tok::TokenKind::MINUS:
                V = Builder.CreateNSWNeg(e);
                break;
        }
    };
    virtual void visit(Grouping &expr) override {
        expr.getExpr()->accept(*this);
    };
    virtual void visit(Literal &expr) override {
        Ty::Type Type = expr.getType();
        if (Type.is(Ty::TypeKind::Bool)) {
            if (expr.getTok().is(tok::TokenKind::kw_true))
                V = ConstantInt::get(mapType(Ty::Type("bool")), 1, true);
            else V = ConstantInt::get(mapType(Ty::Type("bool")), 0, true);
        } else if (expr.getTok().is(tok::TokenKind::kw_null)) {
            V = llvm::UndefValue::get(mapType(Type));
        } else if (Type.get() == "int") {
            int intval;
            expr.getData().getAsInteger(10, intval);
            V = ConstantInt::get(mapType(Ty::Type("int")), intval, true);
        } else if (Type.get() == "float") {
            double temp;
            expr.getData().getAsDouble(temp);
            float fval = static_cast<float>(temp);
            V = ConstantFP::get(mapType(Ty::Type("float")), fval);
        } else if (Type.get() == "double") {
            double dval;
            expr.getData().getAsDouble(dval);
            V = ConstantFP::get(mapType(Ty::Type("double")), dval);
        } else if (Type.is(Ty::TypeKind::String)) {
            // Deal with this later
        }
    };
    virtual void visit(Variable &expr) override {
        auto id = expr.getIdentifier().getIdentifier();
        AllocaInst* alloca = env->getAlloca(id);
        Ty::Type Type = expr.getType();
        if (AddressMode) {
            if (!alloca) {
                alloca = Builder.CreateAlloca(mapType(Type), nullptr, id);
                env->declareAlloca(id, alloca);
            } 
            V = alloca;
        } else
            V = Builder.CreateLoad(mapType(Type), alloca, expr.getData());
    };
    virtual void visit(Logical &expr) override {
        expr.getLeft()->accept(*this);
        Value* left = V;
        BasicBlock* LHS = Curr;
        BasicBlock* RHS;
        BasicBlock* RightBB = createBasicBlock("logical.rhs");
        BasicBlock* Merge = createBasicBlock("logical.merge");
        if (expr.getOp().is(tok::TokenKind::AND)) {
            Builder.CreateCondBr(left, RHS, Merge);

            setCurr(RightBB);
            expr.getRight()->accept(*this);
            Value* right = V;
            RHS = Curr;
            Builder.CreateBr(Merge);

            setCurr(Merge);
            PHINode* phi = Builder.CreatePHI(mapType(Ty::Type("bool")), 2);
            phi->addIncoming(ConstantInt::getFalse(mapType(Ty::Type("bool"))), LHS);
            phi->addIncoming(right, RHS);
            V = phi;
        } else if (expr.getOp().is(tok::TokenKind::OR)) {
            Builder.CreateCondBr(left, Merge, RHS);

            setCurr(RightBB);
            expr.getRight()->accept(*this);
            Value* right = V;
            RHS = Curr;
            Builder.CreateBr(Merge);

            setCurr(Merge);
            PHINode* phi = Builder.CreatePHI(mapType(Ty::Type("bool")), 2);
            phi->addIncoming(ConstantInt::getTrue(mapType(Ty::Type("bool"))), LHS);
            phi->addIncoming(right, RHS);
            V = phi;
        }
    };
    virtual void visit(Assign &expr) override {
        AddressMode = true;
        expr.getLHS()->accept(*this);
        AllocaInst* alloca = static_cast<AllocaInst*>(V);
        AddressMode = false;
        expr.getRHS()->accept(*this);
        Builder.CreateStore(V, alloca);
    };
    virtual void visit(FunExpr &expr) override { 
        llvm::SmallVector<Value*, 256> params;
        for (const auto& p: expr.getParams()) {
            p->accept(*this);
            params.push_back(V);
        }
        Function* callee = M->getFunction(expr.getIdentifier().getIdentifier());
        V = Builder.CreateCall(callee, params);
    };
    virtual void visit(Cast &expr) override {
        expr.getExpr()->accept(*this);
        Ty::Type fromType = expr.getExpr()->getType();
        Ty::Type toType = expr.getType();
        if (toType.get() == "int")
            V = Builder.CreateFPToSI(V, mapType(Ty::Type("int")));
        else if (toType.get() == "float") {
            if (fromType.get() == "int")
                V = Builder.CreateSIToFP(V, mapType(Ty::Type("float")));
            else V = Builder.CreateFPCast(V, mapType(Ty::Type("float")));
        } else if (toType.get() == "double") {
            if (fromType.get() == "int")
                V = Builder.CreateSIToFP(V, mapType(Ty::Type("double")));
            else V = Builder.CreateFPCast(V, mapType(Ty::Type("double")));
        }
    };

    // Statement ASTs
    virtual void visit(Block &stmt) override {
        // Switch Environments
        Environment* prevEnv = env;
        env = stmt.env;
        for (const auto& s: stmt.getStmts())
            s->accept(*this);
        env = prevEnv;
    };
    virtual void visit(Print &stmt) override {
        stmt.getExpr()->accept(*this);
        // FIXME: Runtime function calls for each type
    };
    virtual void visit(Read &stmt) override {
        // FIXME: Runtime function calls for each type
    };
    virtual void visit(Return &stmt) override {
        if (stmt.getExpr()) {
            stmt.getExpr()->accept(*this);
            Builder.CreateRet(V);
        } else
            Builder.CreateRetVoid();
    };
    virtual void visit(Break &stmt) override {
        Builder.CreateBr(BreakBB);
    };
    virtual void visit(Continue &stmt) override {
        Builder.CreateBr(ContBB);
    };
    virtual void visit(If &stmt) override {
        // Switch Environments
        Environment* prevEnv = env;
        env = stmt.ifEnv;

        bool HasElse = stmt.getElseStmt() != nullptr;

        BasicBlock* IfBB = createBasicBlock("if.body");
        BasicBlock* ElseBB =
            HasElse ? createBasicBlock("else.body") : nullptr;
        BasicBlock* AfterIfBB = createBasicBlock("after.if");

        stmt.getExpr()->accept(*this);
        Builder.CreateCondBr(V, IfBB, HasElse ? ElseBB : AfterIfBB);

        setCurr(IfBB);
        stmt.getIfStmt()->accept(*this);
        if (!Curr->getTerminator())
            Builder.CreateBr(AfterIfBB);
        
        if (HasElse) {
            env = stmt.elseEnv;
            setCurr(ElseBB);
            stmt.getElseStmt()->accept(*this);
            if (!Curr->getTerminator())
                Builder.CreateBr(AfterIfBB);
        }

        setCurr(AfterIfBB);
        env = prevEnv;
    };
    virtual void visit(While &stmt) override {
        // Switch Environments and save state
        Environment* prevEnv = env;
        env = stmt.env;
        BasicBlock* PrevContBB = ContBB;
        BasicBlock* PrevBreakBB = BreakBB;

        BasicBlock* CondBB = createBasicBlock("while.cond");
        ContBB = CondBB;
        BasicBlock* BodyBB = createBasicBlock("while.body");
        BasicBlock* AfterWhileBB = createBasicBlock("after.while");
        BreakBB = AfterWhileBB;

        Builder.CreateBr(CondBB);
        setCurr(CondBB);
        stmt.getExpr()->accept(*this);
        Builder.CreateCondBr(V, BodyBB, AfterWhileBB);

        setCurr(BodyBB);
        stmt.getStmt()->accept(*this);
        if (!Curr->getTerminator())
            Builder.CreateBr(CondBB);

        setCurr(AfterWhileBB);
        env = prevEnv;
        ContBB = PrevContBB;
        BreakBB = PrevBreakBB;
    };
    virtual void visit(Until &stmt) override { };
    virtual void visit(For &stmt) override {
        // Switch Environments
        Environment* prevEnv = env;
        env = stmt.env;
        BasicBlock* PrevContBB = ContBB;
        BasicBlock* PrevBreakBB = BreakBB;

        BasicBlock* CondBB = createBasicBlock("for.cond");
        BasicBlock* BodyBB = createBasicBlock("for.body");
        BasicBlock* UpdateBB = createBasicBlock("for.update");
        ContBB = UpdateBB;
        BasicBlock* AfterForBB = createBasicBlock("after.for");
        BreakBB = AfterForBB;

        stmt.getDecl()->accept(*this);
        Builder.CreateBr(CondBB);
        setCurr(CondBB);
        stmt.getCond()->accept(*this);
        Builder.CreateCondBr(V, BodyBB, AfterForBB);

        setCurr(BodyBB);
        stmt.getStmt()->accept(*this);
        if (!Curr->getTerminator())
            Builder.CreateBr(UpdateBB);

        setCurr(UpdateBB);
        stmt.getUpdate()->accept(*this);
        Builder.CreateBr(CondBB);

        setCurr(AfterForBB);
        env = prevEnv;
        ContBB = PrevContBB;
        BreakBB = PrevBreakBB;
    };
    virtual void visit(FunStmt &stmt) override {
        // Save State
        Environment* prevEnv = env;
        Function* PrevFn = Fn;
        BasicBlock* PrevBB = Curr;
        // Switch Environments
        env = stmt.env;
        // Get return type
        Type* RetTy = mapType(stmt.getType());

        // Get Parameter Types
        SmallVector<Type*, 256> FnTypes;
        for (auto& param : stmt.getParams()) {
            FnTypes.push_back(mapType(param->getType()));
        }

        FunctionType* FTy = FunctionType::get(
                RetTy, FnTypes, false);
        Fn = Function::Create(
                FTy, GlobalValue::ExternalLinkage,
                stmt.getIdentifier().getIdentifier(), M);
        BasicBlock* FnBB = createBasicBlock(
                stmt.getIdentifier().getIdentifier());
        setCurr(FnBB);

        // Bind Parameters
        auto argIter = Fn->arg_begin();
        for (auto& param : stmt.getParams()) {
            Argument* arg = argIter++;
            StringRef iden;
            if (auto* assign = llvm::dyn_cast<Assign>(param->getExpr())) {
                if (auto* var = llvm::dyn_cast<Variable>(assign->getLHS()))
                    iden = var->getIdentifier().getIdentifier();
            } else if (auto* var = llvm::dyn_cast<Variable>(param->getExpr()))
                iden = var->getIdentifier().getIdentifier();
            AllocaInst* alloca = Builder.CreateAlloca(
                    mapType(param->getType()),
                    nullptr,
                    iden);
            Builder.CreateStore(arg, alloca);
            env->declareAlloca(iden, alloca);
        }

        stmt.getBody()->accept(*this);
        if (!Curr->getTerminator())
            Builder.CreateRetVoid();

        // Restore State
        Fn = PrevFn;
        setCurr(PrevBB);
        env = prevEnv;
    };
    virtual void visit(ClassStmt &stmt) override {
        llvm::SmallVector<llvm::Type*, 256> Elements;
        for (const auto& F : stmt.getFields())
            Elements.push_back(mapType(F->getType()));
        llvm::Type* T = llvm::StructType::create(
                Elements, stmt.getIdentifier().getIdentifier(), false);
        TypeMap[Ty::Type(stmt.getIdentifier().getIdentifier())] = T;

        for (const auto& Fun : stmt.env->getFuncs())
            Fun.getValue()->accept(*this);
    };
    virtual void visit(Import &stmt) override {
    };
    virtual void visit(Declare &stmt) override {
        stmt.getExpr()->accept(*this);
    };
    virtual void visit(ExprStmt &stmt) override { 
        stmt.getExpr()->accept(*this);
    };
};
}

void CodeGen::compile(const char* Argv0, const char* F, llvm::TargetMachine* TM) {
    M = std::make_unique<Module>(F, Ctx);
    M->setTargetTriple(TM->getTargetTriple().str());
    M->setDataLayout(TM->createDataLayout());
    /* A linux executable generally follows PIE
     * I cant get it to work */
    //M->setPICLevel(llvm::PICLevel::Level::BigPIC);
    //M->setPIELevel(llvm::PIELevel::Level::Large);

    IRVisitor IRV(M.get(), sema.getBaseEnvironment());
    IRV.createMain();
    while (true) {
        std::unique_ptr<AST> Tree = std::move(sema.next());
        if (!Tree) break;
        IRV.run(Tree.get());
        if (auto* func = llvm::dyn_cast<FunStmt>(Tree.get())) {
            std::unique_ptr<FunStmt> F(static_cast<FunStmt*>(Tree.release()));
            llvm::StringRef funcName = F->getIdentifier().getIdentifier();
            if (sema.getBaseEnvironment()->getFunc(funcName) == nullptr)
                sema.getBaseEnvironment()->attachFunc(funcName, std::move(F));
        } else if (auto* cl = llvm::dyn_cast<ClassStmt>(Tree.get())) {
            std::unique_ptr<ClassStmt> C(static_cast<ClassStmt*>(Tree.release()));
            llvm::StringRef className = C->getIdentifier().getIdentifier();
            if (sema.getBaseEnvironment()->getClass(className))
                sema.getBaseEnvironment()->attachClass(className, std::move(C));
        }
    }
    IRV.finishMain();

    if (!TM) {
        llvm::errs() << "Could not create target machine\n";
        return;
    }

    M->print(outs(), nullptr);
}
