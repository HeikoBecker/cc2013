#include "ircreator.h"
#include "sccp_pass.h"

#include "llvm/IR/Module.h"                /* Module */
#include "llvm/IR/Function.h"              /* Function */
#include "llvm/IR/Constant.h"              /* Constant::getNullValue */
#include "llvm/IR/IRBuilder.h"             /* IRBuilder */
#include "llvm/IR/LLVMContext.h"           /* LLVMContext */
#include "llvm/IR/GlobalValue.h"           /* GlobaleVariable, LinkageTypes */
#include "llvm/Analysis/Verifier.h"        /* verifyFunction, verifyModule */
#include "llvm/Support/raw_ostream.h"
#include <llvm/Support/Host.h>

#include "llvm/PassManager.h"
#include "llvm/Transforms/Scalar.h"

#include "../parser/semadecl.h"
#include "../parser/ast.h"

//convenience macros to save some typing time
//create is marked for being inlined!
#define BINCREATE(X) llvm::Value* Codegeneration::IRCreator::X (llvm::Value* lhs,\
                llvm::Value* rhs)
#define BINCREATEL(X) llvm::Value* Codegeneration::IRCreator::X (llvm::Value* lhs,\
                llvm::Value* rhs, int index)
#define UNCREATE(X) llvm::Value* Codegeneration::IRCreator::X(llvm::Value* val)
#define ALLOCF(X) llvm::Value* Codegeneration::IRCreator::X(std::string name)
#define PREPARE(X) this->convert(X, this->USUALTYPE)

Codegeneration::IRCreator::IRCreator(const char* filename):
  M(filename, llvm::getGlobalContext()),
  Builder(M.getContext()), AllocaBuilder(M.getContext()),
  currentFunction(nullptr), currentBreakPoint(nullptr), currentContinuePoint(nullptr)
{

  M.setTargetTriple(llvm::sys::getDefaultTargetTriple());
  mapLabel = std::map<std::string, llvm::BasicBlock* > ();
  //IMPORTANT: CHANGE USUALTYPE HERE IF NECESSARY
  USUALTYPE = Builder.getInt32Ty();
}	

Codegeneration::IRCreator::~IRCreator()
{
  verifyModule(M);
}

void Codegeneration::IRCreator::optimize()
{
    llvm::PassManager PM;
    PM.add(llvm::createPromoteMemoryToRegisterPass());
    PM.add(new SCCP_Pass());
    PM.run(M);
}

void Codegeneration::IRCreator::print(llvm::raw_fd_ostream & out)
{
  M.print(out, nullptr); /* M is a llvm::Module */
}

llvm::Value *Codegeneration::IRCreator::allocateInCurrentFunction(llvm::Type* type)
{
    /* Reset the alloca builder each time before using it
   *
   *   It should not insert any instruction behind the terminator of the entry
   *   block, the easiest way to ensure this is to set it to the begining of
   *   the entry block each time we insert an alloca. */
  AllocaBuilder.SetInsertPoint(AllocaBuilder.GetInsertBlock(),
                               AllocaBuilder.GetInsertBlock()->begin());
  return AllocaBuilder.CreateAlloca(type);
}

llvm::Value* Codegeneration::IRCreator::createLoad(llvm::Value* val) {
  return Builder.CreateLoad(val);
}


void Codegeneration::IRCreator::store(llvm::Value* value, llvm::Value *ptr) {
  Builder.CreateStore(value,ptr);
}

llvm::Function* Codegeneration::IRCreator::startFunction(
    llvm::FunctionType* function_type,
    std::string name,
    bool definition
)
{

  // clear the label map
  mapLabel.clear();

  auto function = llvm::Function::Create(
      function_type,
      llvm::GlobalValue::ExternalLinkage,
      name,
      &M
      );
  if (!definition) {
    return function;
  }
  return startAlreadyDefinedFunction(function, name);
}


llvm::Function* Codegeneration::IRCreator::startAlreadyDefinedFunction(
    llvm::Function* function,
    std::string(name)
    ) 
{
  auto function_basic_block = llvm::BasicBlock::Create(
      M.getContext(), // FIXME: M
      name+"_begin",
      function,
      0 //InsertBefore: inserts at end of surrounding function?
      );
  currentFunction = function;
  Builder.SetInsertPoint(function_basic_block);
  AllocaBuilder.SetInsertPoint(function_basic_block);
  return function;
}

void Codegeneration::IRCreator::finishFunction()
{
  // stol^H^H^H^H borrowed from Johannes' example
  /* All code was emitted,.. but the last block might be empty.
   * If the last block does not end with a terminator statement the simple
   * rules created either dead code or the function is a void function without
   * a return on each path. Either way we need to add a terminator instruction
   * to the last block. The idea is to look at the return type of the current
   * function and emit either a void return or a return with the 'NULL' value
   * for this type */
  if (Builder.GetInsertBlock()->getTerminator() == nullptr) {
    auto CurFuncReturnType = Builder.getCurrentFunctionReturnType();
    if (CurFuncReturnType->isVoidTy()) {
      Builder.CreateRetVoid();
    } else {
      Builder.CreateRet(llvm::Constant::getNullValue(CurFuncReturnType));
    }
  }
  currentFunction = nullptr;
}


void Codegeneration::IRCreator::makeReturn(llvm::Value *value) {
  /* Create the return */
  if (value) {
    auto CurFuncReturnType = Builder.getCurrentFunctionReturnType();
    if (CurFuncReturnType->isIntegerTy() && value->getType()->isIntegerTy()){
      if(CurFuncReturnType != value->getType())
        value = Builder.CreateSExtOrTrunc(value, CurFuncReturnType);
   } else if (! CurFuncReturnType->isIntegerTy() && value->getType()->isIntegerTy())
      value = Builder.CreateIntToPtr(value, CurFuncReturnType);
    else if (CurFuncReturnType->isIntegerTy() && !value->getType()->isIntegerTy())
      value = Builder.CreatePtrToInt(value, CurFuncReturnType);         
    else if (CurFuncReturnType != value->getType())
      value = Builder.CreateBitCast(value, CurFuncReturnType);
    Builder.CreateRet(value);
  } else {
    // if we passed a null pointer to makeReturn, we're in a void function
    Builder.CreateRetVoid();
  }

  /* Always create a new block after a return statement
   *
   *  This will prevent you from inserting code after a block terminator (here
   *  the return instruction), but it will create a dead basic block instead.
   */
  llvm::BasicBlock *ReturnDeadBlock = llvm::BasicBlock::Create(
          Builder.getContext()                   /* LLVMContext &Context */,
          "DEAD_BLOCK"                            /* const Twine &Name="" */,
          currentFunction                         /* Function *Parent=0 */,
          0                                       /* BasicBlock *InsertBefore=0 */);

  /* Insert code following the return in this new 'dead' block */
  Builder.SetInsertPoint(ReturnDeadBlock);
}


llvm::BasicBlock* Codegeneration::IRCreator::makeIfHeader()
{
  using namespace llvm;
  BasicBlock *IfHeaderBlock = BasicBlock::Create(
          Builder.getContext()                    /* LLVMContext &Context */,
          "if-header"                             /* const Twine &Name="" */,
          currentFunction                         /* Function *Parent=0 */,
          0                                       /* BasicBlock *InsertBefore=0 */);
  
  /* Insert an unconditional branch from the current basic block to the header of the IfStmt */
  Builder.CreateBr(IfHeaderBlock);
  /* Set the header of the IfStmt as the new insert point */
  Builder.SetInsertPoint(IfHeaderBlock);
  return IfHeaderBlock;
}

llvm::BasicBlock* Codegeneration::IRCreator::makeBlock(std::string labelName, bool connect) {
  using namespace llvm;
   BasicBlock *labelBlock = BasicBlock::Create(
          Builder.getContext()                    /* LLVMContext &Context */,
          labelName,                                  /* label name */
          currentFunction                         /* Function *Parent=0 */,
          0                                       /* BasicBlock *InsertBefore=0 */);

  if (connect) {
     /* Insert an unconditional branch from the current basic block to the header of the IfStmt */
     Builder.CreateBr(labelBlock);
     /* Set the header of the IfStmt as the new insert point */
     Builder.SetInsertPoint(labelBlock);
   }

  return labelBlock;
}

bool Codegeneration::IRCreator::hasLabel(std::string label) {
  return mapLabel.find(label) != mapLabel.end();
}

void Codegeneration::IRCreator::addLabel(llvm::BasicBlock *block, std::string label) {
  mapLabel[label] = block;
}

llvm::BasicBlock* Codegeneration::IRCreator::getLabelBlock(std::string label) {
  return mapLabel[label];
}

void Codegeneration::IRCreator::makeConditonalBranch(
    llvm::Value* branchCondition,
    llvm::BasicBlock* consequenceBlock,
    llvm::BasicBlock* alternativeBlock)
{
  if(! branchCondition->getType()->isPointerTy())
    branchCondition = Builder.CreateSExt(branchCondition, Builder.getInt32Ty());
  else
    branchCondition = Builder.CreatePtrToInt(branchCondition, Builder.getInt32Ty());
  branchCondition = Builder.CreateICmpNE(branchCondition, llvm::Constant::getNullValue(Builder.getInt32Ty()));
  Builder.CreateCondBr(branchCondition, consequenceBlock, alternativeBlock);
}

llvm::BasicBlock* Codegeneration::IRCreator::getCurrentBlock() {
  return Builder.GetInsertBlock();
}

void Codegeneration::IRCreator::connect(llvm::BasicBlock* to)
{
  auto from = Builder.GetInsertBlock();
  llvm::BranchInst::Create(to, from);
}

llvm::BasicBlock* Codegeneration::IRCreator::connect(llvm::BasicBlock* from, llvm::BasicBlock* to)
{
  if (!from) {
    from = Builder.GetInsertBlock();
  }
  llvm::BranchInst::Create(to, from);
  return from;
}


llvm::GlobalVariable *Codegeneration::IRCreator::makeGlobVar(llvm::Type *type)
{
  return  new llvm::GlobalVariable(
          M                                      /* Module & */,
          type                              /* Type * */,
          false                                   /* bool isConstant */,
          llvm::GlobalValue::CommonLinkage              /* LinkageType */,
          llvm::Constant::getNullValue(type)      /* Constant * Initializer */,
          "TODO"                                /* const Twine &Name = "" */,
          /*--------- We do not need this part (=> use defaults) ----------*/
          0                                       /* GlobalVariable *InsertBefore = 0 */,
          llvm::GlobalVariable::NotThreadLocal          /* ThreadLocalMode TLMode = NotThreadLocal */,
          0                                       /* unsigned AddressSpace = 0 */,
          false                                   /* bool isExternallyInitialized = false */);
   
}

/*
 * Self explanatory binary expression functions. Special cases are annotated.
 * Casting is done with the PREPARE makro, which makes use of the convert 
 * function so that we do not create unnecessary casts.
 */
BINCREATE(createAdd) {
        lhs = PREPARE(lhs);
        rhs = PREPARE(rhs);
	return Builder.CreateAdd(lhs,rhs);
}

/*
 * Produces pointer arithmetic expressions:
 * x+y must create an offset of y* sizeof (type of x). This is done by using a
 * GEP
 */
BINCREATE(createPAdd) {
        if(rhs->getType() !=  Builder.getInt32Ty())
                rhs = Builder.CreateSExtOrTrunc(rhs,Builder.getInt32Ty());
        return Builder.CreateGEP(lhs, rhs);
}

/*
 * Same as for createPAdd
 */
BINCREATE(createPMinus){
        if(rhs->getType() != Builder.getInt32Ty())
                rhs = Builder.CreateSExtOrTrunc(rhs, Builder.getInt32Ty());
        // we have to negate subtract, as rhs is positive in lhs - rhs
        rhs = Builder.CreateNeg(rhs);
        return Builder.CreateGEP(lhs, rhs);
}

/*
 * We need to produce the number of elements between the two pointers.
 * As llvm offers a method to compute PtrDiff and we already did type checking 
 * it is safe to use it here.
 */
BINCREATE(createPPMinus) {
        llvm::Value* val = Builder.CreatePtrDiff(lhs, rhs);
        return convert(val, Builder.getInt32Ty());
}


BINCREATE(createMinus) {
        lhs = PREPARE(lhs);
        rhs = PREPARE(rhs);
	return Builder.CreateSub(lhs, rhs);
}

/*
 * We use signed less than for the less than operator as usual arithmetic 
 * conversions imply an implicit threatment of all values as i32 integer 
 * which are signed in our C subset
 */
BINCREATE(createLess) {
  if(this->isVoidP(lhs->getType())){
    if(! this->isVoidP(rhs->getType())){ //right is no void pointer --> cast
      //right could be a NullPointerConst
      if(rhs->getType() == Builder.getInt32Ty())
        lhs = Builder.CreatePtrToInt(lhs, Builder.getInt32Ty());
      else
        lhs = Builder.CreateBitCast(lhs, rhs->getType());
    }
  }else{ //left is no void pointer 
    if( this->isVoidP(rhs->getType())) //right is void pointer --> cast to lhs
      //left could be a nullptr const
      if(lhs->getType() == Builder.getInt32Ty())
        rhs = Builder.CreatePtrToInt(rhs, Builder.getInt32Ty());
      else
        rhs = Builder.CreateBitCast(rhs, lhs->getType());
    else { //neither is void pointer

      if(! lhs->getType()->isPointerTy()){ //the left one is no pointer
        if (rhs->getType()->isPointerTy()){ // the right one is a pointer
          lhs = this->convert(lhs, rhs->getType()); //--> lhs is a nullptr
        }else{ //none is a pointer --> usual arithmetic conversions
          lhs = PREPARE(lhs);
          rhs = PREPARE(rhs);
        }
      }else{ //left is a pointer
        if(! rhs->getType()->isPointerTy()) //right is no pointer
          rhs = this->convert(rhs, lhs->getType()); //rhs is a nullptr
      }
    }
  }
  if (lhs->getType() != rhs->getType()) {
    lhs = Builder.CreateBitCast(lhs, rhs->getType());
  }
  auto as_i1 = Builder.CreateICmpSLT(lhs,rhs);
  // the comparision returns an i1, but what we need is a int32
  return Builder.CreateZExtOrTrunc(as_i1, Builder.getInt32Ty());
}

BINCREATE(createMult) {
        lhs = PREPARE(lhs);
        rhs = PREPARE(rhs);
	return Builder.CreateMul(lhs, rhs);
}

BINCREATE(createUnequal){
 if(this->isVoidP(lhs->getType())){
    if(! this->isVoidP(rhs->getType())){ //right is no void pointer --> cast
      //right could be a NullPointerConst
      if(rhs->getType() == Builder.getInt32Ty())
        lhs = Builder.CreatePtrToInt(lhs, Builder.getInt32Ty());
      else
        lhs = Builder.CreateBitCast(lhs, rhs->getType());
    }
  }else{ //left is no void pointer 
    if( this->isVoidP(rhs->getType())) //right is void pointer --> cast to lhs
      //left could be a nullptr const
      if(lhs->getType() == Builder.getInt32Ty())
        rhs = Builder.CreatePtrToInt(rhs, Builder.getInt32Ty());
      else
        rhs = Builder.CreateBitCast(rhs, lhs->getType());
    else { //neither is void pointer

      if(! lhs->getType()->isPointerTy()){ //the left one is no pointer
        if (rhs->getType()->isPointerTy()){ // the right one is a pointer
          lhs = this->convert(lhs, rhs->getType()); //--> lhs is a nullptr
        }else{ //none is a pointer --> usual arithmetic conversions
          lhs = PREPARE(lhs);
          rhs = PREPARE(rhs);
        }
      }else{ //left is a pointer
        if(! rhs->getType()->isPointerTy()) //right is no pointer
          rhs = this->convert(rhs, lhs->getType()); //rhs is a nullptr
      }
    }
   }
 if (lhs->getType() != rhs->getType()) {
   lhs = Builder.CreateBitCast(lhs, rhs->getType());
 }
 auto as_i1 = Builder.CreateICmpNE(lhs,rhs);
  // the comparision returns an i1, but what we need is a int32
  return Builder.CreateZExtOrTrunc(as_i1, Builder.getInt32Ty());
}

BINCREATE(createEqual){
 if(this->isVoidP(lhs->getType())){
    if(! this->isVoidP(rhs->getType())){ //right is no void pointer --> cast
      //right could be a NullPointerConst
      if(rhs->getType() == Builder.getInt32Ty())
        lhs = Builder.CreatePtrToInt(lhs, Builder.getInt32Ty());
      else
        lhs = Builder.CreateBitCast(lhs, rhs->getType());
    }
  }else{ //left is no void pointer 
    if( this->isVoidP(rhs->getType())) //right is void pointer --> cast to lhs
      //left could be a nullptr const
      if(lhs->getType() == Builder.getInt32Ty())
        rhs = Builder.CreatePtrToInt(rhs, Builder.getInt32Ty());
      else
        rhs = Builder.CreateBitCast(rhs, lhs->getType());
    else { //neither is void pointer

      if(! lhs->getType()->isPointerTy()){ //the left one is no pointer
        if (rhs->getType()->isPointerTy()){ // the right one is a pointer
          lhs = this->convert(lhs, rhs->getType()); //--> lhs is a nullptr
        }else{ //none is a pointer --> usual arithmetic conversions
          lhs = PREPARE(lhs);
          rhs = PREPARE(rhs);
        }
      }else{ //left is a pointer
        if(! rhs->getType()->isPointerTy()) //right is no pointer
          rhs = this->convert(rhs, lhs->getType()); //rhs is a nullptr
      }
    }
   }
 if (lhs->getType() != rhs->getType()) {
   lhs = Builder.CreateBitCast(lhs, rhs->getType());
 }
  auto as_i1 =  Builder.CreateICmpEQ(lhs,rhs);
  // the comparision returns an i1, but what we need is a int32
  return Builder.CreateZExtOrTrunc(as_i1, Builder.getInt32Ty());
}

BINCREATE(createLogAnd){
  bool rhsPointer = rhs->getType()->isPointerTy();
  if(lhs->getType()->isPointerTy()){
    lhs = (rhsPointer? lhs : Builder.CreatePtrToInt(lhs, USUALTYPE));
    rhs = (rhsPointer? rhs :PREPARE(rhs));
  }else{
    rhs = (rhsPointer? Builder.CreatePtrToInt(rhs, USUALTYPE):PREPARE(rhs));
    lhs = PREPARE(lhs);
  }
  return Builder.CreateAnd(lhs, rhs);
}

BINCREATE(createLogOr){
  bool rhsPointer = rhs->getType()->isPointerTy();
  if(lhs->getType()->isPointerTy()){
    lhs = (rhsPointer? lhs : Builder.CreatePtrToInt(lhs, USUALTYPE));
    rhs = (rhsPointer? rhs :PREPARE(rhs));
  }else{
    rhs = (rhsPointer? Builder.CreatePtrToInt(rhs, USUALTYPE):PREPARE(rhs));
    lhs = PREPARE(lhs);
  }	return Builder.CreateOr(lhs,rhs);
}

BINCREATE(createArrayAccess){
        llvm::Value* val= Builder.CreateGEP(lhs, rhs);
        return Builder.CreateLoad(val);
}

/*
 * This method has exacty the same behaviour as createAccess as all 
 * loading from adresses has to be done before this method is called.
 */
BINCREATEL(createPointerAccess) {
        UNUSED(rhs);
        //Loading was done before when computing the rvalue of lhs.
       std::vector<llvm::Value*> indexes;
       indexes.push_back(Builder.getInt32(0));
       indexes.push_back(Builder.getInt32(index));
       //Compute the GEP
       llvm::Value *GEP = Builder.CreateInBoundsGEP(lhs, indexes);
       llvm::LoadInst *var = Builder.CreateLoad(GEP);
       return var;
}

/*
 * Index computation has been done in the codegeneration method for Binary 
 * Expressions as we need acces to both names of the acces operand which are 
 * saved in the corresponding expression trees and not in their llvm 
 * representation. Only thing we need to do then is acces the right element by
 * using an indexvector.
 */
BINCREATEL(createAccess) {
        UNUSED(rhs);
        std::vector<llvm::Value*>indexes;
        indexes.push_back(Builder.getInt32(0));
        indexes.push_back(Builder.getInt32(index));
        //Compute the GEP
        llvm::Value *GEP = Builder.CreateInBoundsGEP(lhs, indexes);
        llvm::LoadInst *var = Builder.CreateLoad(GEP);
        return var;
}

/*
 * Assignments can be done directly with the builder. Only thing necessary is
 * casting the value to the type of the variable, if they dont match, as
 * semantic checking already validated it, this should not cause troubles.
 */
llvm::Value* Codegeneration::IRCreator::createAssign(llvm::Value* lhs, 
                llvm::Value* rhs, llvm::Type* type) {
  if(this->isVoidP(rhs->getType())){
    rhs = Builder.CreateBitCast(rhs, type);
  }else if(this->isVoidPP(lhs->getType())){
    lhs = Builder.CreateBitCast(lhs, llvm::PointerType::getUnqual(rhs->getType()));    
  }else{ //No void pointers involved
  rhs = this->convert(rhs, type);
  }
  store(rhs,lhs);
  return rhs;

}

/*
 * Same as getMemberAddress, as loading of the struct address has been done by 
 * value computation for lhs before.
 */
BINCREATEL(getAddressfromPointer){
        UNUSED(rhs);
        std::vector<llvm::Value *> indexes;
        indexes.push_back(Builder.getInt32(0));
        indexes.push_back(Builder.getInt32(index));
        //Compute the GEP & return it
        llvm::Value *GEP = Builder.CreateInBoundsGEP(lhs, indexes);
        return GEP;
}

/*
 * Used to create the lvalue of a struct field.
 * It behaves like the createAcces and createPointerAccess methods as we need
 * the same place. The only difference is that there is no load instruction as
 * we want the address toi store the new value there.
 */
BINCREATEL(getMemberAddress){
        UNUSED(rhs);
        std::vector<llvm::Value *> indexes;
        indexes.push_back(Builder.getInt32(0));
        indexes.push_back(Builder.getInt32(index));
        //Compute the GEP & return it
        llvm::Value *GEP = Builder.CreateInBoundsGEP(lhs, indexes);
        return GEP;
}

/*
 * Array accesses are just another style of writing pointer arithmetics.
 * A[1] is the same as A + 1 
 */
BINCREATEL(getArrayPosition) {
        UNUSED(index);
        llvm::Value* val = Builder.CreateLoad(lhs);
        return Builder.CreateGEP(val,rhs);
//        UNUSED(index);
//        lhs = Builder.CreateSExt(lhs, Builder.getInt32Ty());
//        rhs = Builder.CreateSExt(rhs, Builder.getInt32Ty());
//	return Builder.CreateAdd(lhs,rhs);
}

UNCREATE(createLogNeg) {
  if(val->getType()->isPointerTy()){
    auto type = val->getType();
    val = Builder.CreatePtrToInt(val, Builder.getInt32Ty());
    val = Builder.CreateNot(val);
    return Builder.CreateIntToPtr(val, type);
   }else{
    return Builder.CreateNot(val);
   }
}

UNCREATE(createNeg) { 
  if(val->getType()->isPointerTy()){
    auto ty = val->getType();
    val = Builder.CreatePtrToInt(val, Builder.getInt32Ty());
    val = Builder.CreateNeg(val);
    return Builder.CreateIntToPtr(val, ty);
  }else {
    return Builder.CreateNeg(val);
  }
}

/*
 * Taken from the ircreation slides :D
 */
UNCREATE(createDeref) { 
        return Builder.CreateLoad(val);
}

llvm::Value* Codegeneration::IRCreator::createSizeof(llvm::Type* type)
{
  auto val = llvm::ConstantExpr::getSizeOf(type);
  return Builder.CreateSExtOrTrunc(val,  Builder.getInt32Ty());
}

llvm::Value* Codegeneration::IRCreator::loadVariable(
                llvm::Value *val) {
  return Builder.CreateLoad(val);
}

/*
 * Allocation functions. They produce the llvm value, given the internal value
 */
ALLOCF(allocLiteral) {
  // FIXME: maybe we should have done this in the lexer
  auto unescaped = name;
  return Builder.CreateGlobalStringPtr(llvm::StringRef(unescaped));
}

llvm::Value* Codegeneration::IRCreator::allocChar (char val) {
  return Builder.getInt8(val);
}

llvm::Value* Codegeneration::IRCreator::allocInt(int val){
  return Builder.getInt32(val);
}

llvm::Value* Codegeneration::IRCreator::allocNullptr(llvm::Type* type) {
  if (type->isPointerTy()) {
    return llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(type));
  } else if (type->isIntegerTy()) {
    return Builder.getInt32(0);
  } else {
    return llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(Builder.getInt32Ty()));
  }
}

llvm::Value* Codegeneration::IRCreator::makeSelect(Parsing::SubExpression cond,
                Parsing::SubExpression lhs, Parsing::SubExpression rhs) {
        llvm::BasicBlock* left = getControlFlowBlock();
        llvm::BasicBlock* right = getControlFlowBlock();
        llvm::BasicBlock* term = getControlFlowBlock();
        llvm::Value* branch = cond->emit_rvalue(this);
        makeConditonalBranch(branch, left, right);
        setCurrentBasicBlock(left);
        llvm::Value* vall = lhs->emit_rvalue(this);
        vall = Builder.CreateSExt(vall, Builder.getInt32Ty());
        Builder.CreateBr(term);
        setCurrentBasicBlock(right);
        llvm::Value* valr = rhs->emit_rvalue(this);
        valr = Builder.CreateSExt(valr, Builder.getInt32Ty());
        Builder.CreateBr(term);
        setCurrentBasicBlock(term);
        llvm::Value* phi = Builder.CreatePHI(Builder.getInt32Ty(), 2);
        return phi;
}

llvm::Value* Codegeneration::IRCreator::makeSelectLV(Parsing::SubExpression cond,
                Parsing::SubExpression lhs, Parsing::SubExpression rhs){
        llvm::BasicBlock* left = getControlFlowBlock();
        llvm::BasicBlock* right = getControlFlowBlock();
        llvm::BasicBlock* term = getControlFlowBlock();
        llvm::Value* branch = cond->emit_rvalue(this);
        makeConditonalBranch(branch, left, right);
        setCurrentBasicBlock(left);
        llvm::Value* vall = lhs->emit_lvalue(this);
        vall = Builder.CreateSExt(vall, Builder.getInt32Ty());
        Builder.CreateBr(term);
        setCurrentBasicBlock(right);
        llvm::Value* valr = rhs->emit_lvalue(this);
        valr = Builder.CreateSExt(valr, Builder.getInt32Ty());
        Builder.CreateBr(term);
        setCurrentBasicBlock(term);
        llvm::Value* phi = Builder.CreatePHI(Builder.getInt32Ty(), 2);
        return phi;
}


llvm::Value* Codegeneration::IRCreator::makePhi(
    llvm::BasicBlock* consequenceBlock,
    llvm::Value* consequenceValue,
    llvm::BasicBlock* alternativeBlock,
    llvm::Value* alternativeValue
    )
{
  auto phi_node = Builder.CreatePHI(consequenceValue->getType(), 2, "merge");
  phi_node->addIncoming(consequenceValue, consequenceBlock);
  phi_node->addIncoming(alternativeValue, alternativeBlock);
  return phi_node;
}

/*
 * A functions arguments can have different types than it expects in llvm.
 * We checked the semantics before, so it is safe to cast them in the correct
 * type.
 */
llvm::Value* Codegeneration::IRCreator::createFCall(llvm::Value* func,
                std::vector<llvm::Value*> params,
                std::vector<llvm::Type*> paramTypes) { 
  std::vector<llvm::Value*> vals;
  
  //convert the arguments if necessary
  for(unsigned long i = 0; i < params.size(); ++i)
      vals.push_back(convert(params[i], paramTypes[i]));

  //produce the function call with the parameters
  if (func->isDereferenceablePointer()) {
    func = this->createLoad(func);
  }
  return Builder.CreateCall(func, vals);
}


llvm::BasicBlock* Codegeneration::IRCreator::getControlFlowBlock()
{
  return llvm::BasicBlock::Create(
      Builder.getContext(),
      "shortCircuitingBlock",
      currentFunction, // insert inside current function
      nullptr
      );
}
void Codegeneration::IRCreator::setCurrentBasicBlock(llvm::BasicBlock* bb)
{
  Builder.SetInsertPoint(bb);
}

void Codegeneration::IRCreator::setCurrentBreakPoint(llvm::BasicBlock* block) {
  currentBreakPoint = block;
}

void Codegeneration::IRCreator::setCurrentContinuePoint(llvm::BasicBlock* block) {
  currentContinuePoint = block;
}

void Codegeneration::IRCreator::makeBreak() {
  connect(currentBreakPoint);
}

void Codegeneration::IRCreator::makeContinue() {
  connect(currentContinuePoint);
}

/*
 *  Converts one of our type classes to the corresponding LLVM Type
 */
llvm::Type* Codegeneration::IRCreator::semantic_type2llvm_type(
    const Parsing::SemanticDeclarationNode semantic_type) {
  llvm::Type *llvm_type = nullptr;
  switch(semantic_type->type()){
    case Semantic::Type::INT:
      llvm_type = Builder.getInt32Ty();
      break;

    case Semantic::Type::CHAR:
      llvm_type = Builder.getInt8Ty();
      break;
                              
    case Semantic::Type::VOID:
      llvm_type = Builder.getVoidTy();
      break;
                              
    case Semantic::Type::ARRAY:
    case Semantic::Type::POINTER:
      {
        auto pointer_type =
          std::static_pointer_cast<Parsing::PointerDeclaration>(semantic_type);	
        auto pointee = pointer_type->pointee();
        if (pointee->type() == Semantic::Type::VOID) {
          llvm_type = llvm::PointerType::getUnqual(Builder.getInt8Ty());
          break;
        }
        llvm_type = llvm::PointerType::getUnqual(
            semantic_type2llvm_type(pointee)
            );
      break;
      }
                                 
    case Semantic::Type::STRUCT:
      {
        auto structType =
          std::static_pointer_cast<Parsing::StructDeclaration>(semantic_type);
        if (*structType->llvm_type) {
          llvm_type = *structType->llvm_type;
          break;
        }
        auto struct_type = llvm::StructType::create(
            this->Builder.getContext(),
            structType->toString()
            );
        *structType->llvm_type = struct_type;
        /* TODO: handle non primitive types*/
        std::vector<llvm::Type *> member_types;
        // TODO: use transform
        if (structType->members().size()) {
          for (auto member: structType->members()) {
            member_types.push_back(semantic_type2llvm_type(member.second));
          }
        struct_type->setBody(member_types);
        }
        llvm_type = struct_type;
      }
      break;
    case Semantic::Type::FUNCTION:
      {
      auto function_type_ = std::static_pointer_cast<Parsing::FunctionDeclaration>(semantic_type);
      // lookup the return type and set it correctly
      auto return_type_ = function_type_->returnType();
      auto return_type = this->semantic_type2llvm_type(return_type_); 
      /*************************************/
      /* TODO: set the correct parameter types */
      auto parameter_types = std::vector<llvm::Type *>();
      parameter_types.reserve(function_type_->parameter().size());
      // iterate over parameter_types and push corresponding LLVM type into vector
      for (auto p: function_type_->parameter()) {
            auto argument_type = p;
            if (p->type() == Semantic::Type::FUNCTION) {
              argument_type = std::make_shared<Parsing::PointerDeclaration>(
                  0,
                  argument_type
              );
            }
        parameter_types.push_back(this->semantic_type2llvm_type(argument_type));
      }
      /*************************************/
      llvm::FunctionType* function_type = llvm::FunctionType::get(
          return_type,
          parameter_types,
          /*isVarArg=*/false); 
      /* TODO: set the names of the function arguments
       * byiterating over them and calling setName*/
      // TODO: retrive function argument names
      /********************************************/
      llvm_type = function_type;
      break;
      }
    default:
      llvm_type = Builder.getInt32Ty();
  }
  return llvm_type;
}

/*
 * Given the two expression nodes, where lhs MUST be a struct or a pointer to 
 * it and rhs MUST be a variable, the function computes the correct index for 
 * the GEP pointer
 */
int Codegeneration::IRCreator::computeIndex (Parsing::SubExpression lhs, 
                Parsing::SubExpression rhs){
  Parsing::SemanticDeclarationNode type = lhs->getType();
  //It could also be a pointer based acces with ->
  //check this
  if(type->type() == Semantic::Type::POINTER){
    auto pointer = std::static_pointer_cast<Parsing::PointerDeclaration> (type);
    //save the struct type in the type field
    type = pointer->pointee();
  } //now we can continue
  auto  structtype = std::static_pointer_cast<Parsing::StructDeclaration> (type);
  auto members = structtype->members();
  auto it = members.begin();
  auto  rhsAsVar = std::static_pointer_cast<Parsing::VariableUsage> (rhs);
  //our indexes start with 0 as in the example file
  int i = 0;
  //now compute the index with the compareTypes function
  while (! (Semantic::compareTypes(it->second, rhs->getType())) && 
         ! (it->first == rhsAsVar->name)){
    ++it;
    ++i;
  }
  return i;
}

/*
 * Method for easy type conversion. Argument vals type is compared to the
 * expected type. If they match, no type conversion is done. Otherwise val
 * will be casted and the resulting value will be returned.
 */
llvm::Value* Codegeneration::IRCreator::convert(llvm::Value* val, llvm::Type* t){
  if(val->getType() != t )
    return Builder.CreateSExtOrTrunc(val, t);
  else
    return val;
} 

llvm::Value* Codegeneration::IRCreator::convert(
    llvm::Value* val,
    Parsing::SemanticDeclarationNode s)
{
  auto llvm_type = semantic_type2llvm_type(s);
  return convert(val, llvm_type);
}

bool Codegeneration::IRCreator::isVoidPP (llvm::Type *type){
  if(type->isPointerTy())
        return this->isVoidP(type->getPointerElementType());
  else
    return false;
}

bool Codegeneration::IRCreator::isVoidP (llvm::Type *type){
    if(type->isPointerTy())
            return type->getPointerElementType() == Builder.getInt8Ty();
    return false;
}
