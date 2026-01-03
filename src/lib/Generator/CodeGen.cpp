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
    Type* VoidTy;
    Type* BoolTy;
    Type* Int32Ty;
    Type* FloatTy;
    Type* DoubleTy;
    PointerType *PtrTy;
    Value* PrintStr;
    Value* ReadStr;
    Value* V;
    Function* Fn;
    BasicBlock* Curr;

    Environment* env;
    BasicBlock* ContBB;
    BasicBlock* BreakBB;

    BasicBlock* createBasicBlock(const Twine& name,
                      BasicBlock* InsertBefore = nullptr) {
        return BasicBlock::Create(M->getContext(), name, Fn, InsertBefore);
    }

    void setCurr(BasicBlock* BB) {
        Curr = BB;
        Builder.SetInsertPoint(Curr);
    }

    Type* mapType(std::string T) {
        if (T == "bool") return BoolTy;
        else if (T == "int") return Int32Ty;
        else if (T == "float") return FloatTy;
        else if (T == "double") return DoubleTy;
        else if (T == "void") return VoidTy;
        else if (T == "string"); // FIXME: Add string
        return nullptr;
    }

public:
    IRVisitor(Module* M, Environment* env) : M(M), env(env), Builder(M->getContext()) {
        VoidTy = Type::getVoidTy(M->getContext());
        BoolTy = Type::getInt1Ty(M->getContext());
        Int32Ty = Type::getInt32Ty(M->getContext());
        FloatTy = Type::getFloatTy(M->getContext());
        DoubleTy = Type::getDoubleTy(M->getContext());
        PtrTy = PointerType::getUnqual(M->getContext());

        //PrintFTy = FunctionType::get(Builder.getInt32Ty(), Builder.getInt8PtrTy(), true);
        //PrintF = M->getOrInsertFunction("printf", PrintFTy);
        //ScanFTy = FunctionType::get(Builder.getInt32Ty(), Builder.getInt8PtrTy(), true);
        //ScanF = M->getOrInsertFunction("scanf", ScanFTy);
    }

    void createMain() {
        FunctionType* MainFty = FunctionType::get(
                Int32Ty, {Int32Ty, PtrTy}, false);
        Fn = Function::Create(
                MainFty, GlobalValue::ExternalLinkage, "main", M);
        BasicBlock* MainBB = createBasicBlock("entry");
        setCurr(MainBB);
    }

    void finishMain() {
        Builder.CreateRet(ConstantInt::get(Int32Ty, 1, true));
    }

    void run(AST* Tree) {
        Tree->print();
        std::cout << "\n";
        Tree->accept(*this);
        //Builder.CreateCall(PrintF, {PrintStr, V});
    }

    // Expression ASTs
    virtual void visit(BinaryOp &expr) override {
        expr.getLeft()->accept(*this);
        Value* left = V;
        expr.getRight()->accept(*this);
        Value* right = V;
        std::string Type = expr.getType();
        switch (expr.getOp().getKind()) {
            case tok::TokenKind::PLUS:
                if (Type == "string");
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
                if (Type == "int") V = Builder.CreateSDiv(left, right);
                else V = Builder.CreateFDiv(left, right);
                break;
            case tok::TokenKind::CARET:
                if (Type == "int") {
                    // FIXME: No build in power for ints. Runtime library it probably
                } else if (Type == "float") {
                    Function* PowFn = Intrinsic::getDeclaration(
                            M, Intrinsic::pow, {FloatTy});
                    V = Builder.CreateCall(PowFn, {left, right});
                } else if (Type == "double") {
                    Function* PowFn = Intrinsic::getDeclaration(
                            M, Intrinsic::pow, {DoubleTy});
                    V = Builder.CreateCall(PowFn, {left, right});
                }
                break;
            case tok::TokenKind::PERCENT:
                V = Builder.CreateSRem(left, right);
                break;
            case tok::TokenKind::EQ:
                if (Type == "int" || Type == "bool")
                    V = Builder.CreateICmpEQ(left, right);
                else if (Type == "float" || Type == "double")
                    V = Builder.CreateFCmpOEQ(left, right);
                else if (Type == "string")
                    // FIXME: Runtime library for this
                    ;
                break;
            case tok::TokenKind::NEQ:
                if (Type == "int" || Type == "bool")
                    V = Builder.CreateICmpNE(left, right);
                else if (Type == "float" || Type == "double")
                    V = Builder.CreateFCmpONE(left, right);
                else if (Type == "string")
                    // FIXME: Runtime library for this
                    ;
                break;
            case tok::TokenKind::LESS:
                if (Type == "int" || Type == "bool")
                    V = Builder.CreateICmpSLT(left, right);
                else if (Type == "float" || Type == "double")
                    V = Builder.CreateFCmpOLT(left, right);
                else if (Type == "string")
                    // FIXME: Runtime library for this
                    ;
                break;
            case tok::TokenKind::LEQ:
                if (Type == "int" || Type == "bool")
                    V = Builder.CreateICmpSLE(left, right);
                else if (Type == "float" || Type == "double")
                    V = Builder.CreateFCmpOLE(left, right);
                else if (Type == "string")
                    // FIXME: Runtime library for this
                    ;
                break;
            case tok::TokenKind::GREATER:
                if (Type == "int" || Type == "bool")
                    V = Builder.CreateICmpSGT(left, right);
                else if (Type == "float" || Type == "double")
                    V = Builder.CreateFCmpOGT(left, right);
                else if (Type == "string")
                    // FIXME: Runtime library for this
                    ;
                break;
            case tok::TokenKind::GEQ:
                if (Type == "int" || Type == "bool")
                    V = Builder.CreateICmpSGE(left, right);
                else if (Type == "float" || Type == "double")
                    V = Builder.CreateFCmpOGE(left, right);
                else if (Type == "string")
                    // FIXME: Runtime library for this
                    ;
                break;
        }
    };
    virtual void visit(UnaryOp &expr) override {
        expr.getExpr()->accept(*this);
        Value* e = V;
        std::string Type = expr.getType();
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
        std::string Type = expr.getType();
        if (Type == "bool") {
            if (expr.getTok().is(tok::TokenKind::kw_true))
                V = ConstantInt::get(BoolTy, 1, true);
            else V = ConstantInt::get(BoolTy, 0, true);
        } else if (Type == "int") {
            int intval;
            expr.getData().getAsInteger(10, intval);
            V = ConstantInt::get(Int32Ty, intval, true);
        } else if (Type == "float") {
            double temp;
            expr.getData().getAsDouble(temp);
            float fval = static_cast<float>(temp);
            V = ConstantFP::get(FloatTy, fval);
        } else if (Type == "double") {
            double dval;
            expr.getData().getAsDouble(dval);
            V = ConstantFP::get(DoubleTy, dval);
        } else if (Type == "string") {
            // Deal with this later
        }
    };
    virtual void visit(Variable &expr) override {
        AllocaInst* id = env->getAlloca(expr.getData());
        V = Builder.CreateLoad(mapType(expr.getType()), id, expr.getData());
    };
    virtual void visit(Logical &expr) override {
        expr.getLeft()->accept(*this);
        Value* left = V;
        BasicBlock* RHS = createBasicBlock("logical.rhs");
        BasicBlock* Merge = createBasicBlock("logical.merge");
        if (expr.getOp().is(tok::TokenKind::AND)) {
            Builder.CreateCondBr(left, RHS, Merge);

            setCurr(RHS);
            expr.getRight()->accept(*this);
            Value* right = V;
            Builder.CreateBr(Merge);

            setCurr(Merge);
            PHINode* phi = Builder.CreatePHI(BoolTy, 2);
            phi->addIncoming(ConstantInt::getFalse(BoolTy), Curr);
            phi->addIncoming(right, RHS);
            V = phi;
        } else if (expr.getOp().is(tok::TokenKind::OR)) {
            Builder.CreateCondBr(left, Merge, RHS);

            setCurr(RHS);
            expr.getRight()->accept(*this);
            Value* right = V;
            Builder.CreateBr(Merge);

            setCurr(Merge);
            PHINode* phi = Builder.CreatePHI(BoolTy, 2);
            phi->addIncoming(ConstantInt::getTrue(BoolTy), Curr);
            phi->addIncoming(right, RHS);
            V = phi;
        }
    };
    virtual void visit(Assign &expr) override {
        auto id = expr.getIdentifier().getIdentifier();
        AllocaInst* alloca = env->getAlloca(id);
        std::string Type = expr.getType();
        if (!alloca) {
            alloca = Builder.CreateAlloca(mapType(Type), nullptr, id);
            // FIXME: Add strings later
            env->declareAlloca(id, alloca);
        }
        expr.getExpr()->accept(*this);
        Builder.CreateStore(V, alloca);
    };
    virtual void visit(FunExpr &expr) override { 
        // TODO: Create call to function 
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
        std::string fromType = expr.getExpr()->getType();
        std::string toType = expr.getType();
        if (toType == "int")
            V = Builder.CreateFPToSI(V, Int32Ty);
        else if (toType == "float") {
            if (fromType == "int")
                V = Builder.CreateSIToFP(V, FloatTy);
            else V = Builder.CreateFPCast(V, FloatTy);
        } else if (toType == "double") {
            if (fromType == "int")
                V = Builder.CreateSIToFP(V, DoubleTy);
            else V = Builder.CreateFPCast(V, DoubleTy);
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
        Builder.CreateBr(AfterIfBB);
        
        if (HasElse) {
            env = stmt.elseEnv;
            setCurr(ElseBB);
            stmt.getElseStmt()->accept(*this);
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
        ContBB = CondBB;
        BasicBlock* BodyBB = createBasicBlock("for.body");
        BasicBlock* AfterForBB = createBasicBlock("after.for");
        BreakBB = AfterForBB;

        stmt.getDecl()->accept(*this);
        Builder.CreateBr(CondBB);
        setCurr(CondBB);
        stmt.getCond()->accept(*this);
        Builder.CreateCondBr(V, BodyBB, AfterForBB);

        setCurr(BodyBB);
        stmt.getStmt()->accept(*this);
        stmt.getUpdate()->accept(*this);
        if (!Curr->getTerminator())
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
        Type* RetTy = mapType(stmt.getType().getLexeme().str());

        // Get Parameter Types
        SmallVector<Type*, 256> FnTypes;
        for (auto& param : stmt.getParams()) {
            FnTypes.push_back(mapType(param->getType().getLexeme().str()));
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
            if (llvm::dyn_cast<Assign>(param->getExpr()))
                iden = static_cast<Assign*>(param->getExpr())->getIdentifier().getIdentifier();
            else
                iden = static_cast<Variable*>(param->getExpr())->getData();
            AllocaInst* alloca = Builder.CreateAlloca(
                    mapType(param->getType().getLexeme().str()),
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
        }
    }
    IRV.finishMain();

    if (!TM) {
        llvm::errs() << "Could not create target machine\n";
        return;
    }

    M->print(outs(), nullptr);
}
