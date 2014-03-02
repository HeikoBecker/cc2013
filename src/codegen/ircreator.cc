#include "ircreator.h"

#include "llvm/IR/Module.h"                /* Module */
#include "llvm/IR/Function.h"              /* Function */
#include "llvm/IR/Constant.h"              /* Constant::getNullValue */
#include "llvm/IR/IRBuilder.h"             /* IRBuilder */
#include "llvm/IR/LLVMContext.h"           /* LLVMContext */
#include "llvm/IR/GlobalValue.h"           /* GlobaleVariable, LinkageTypes */
#include "llvm/Analysis/Verifier.h"        /* verifyFunction, verifyModule */
#include "llvm/Support/raw_ostream.h"

//convenience macros to save some typing time
//create is marked for being inlined!
#define BINCREATE(X) llvm::Value* Codegeneration::IRCreator::X (llvm::Value* lhs,\
                llvm::Value* rhs)
#define BINCREATEL(X) llvm::Value* Codegeneration::IRCreator::X (llvm::Value* lhs,\
                llvm::Value* rhs, int index)
#define UNCREATE(X) llvm::Value* Codegeneration::IRCreator::X(llvm::Value* val)
#define ALLOCF(X) llvm::Value* Codegeneration::IRCreator::X(std::string name)

Codegeneration::IRCreator::IRCreator(const char* filename):
  M(filename, llvm::getGlobalContext()),
  Builder(M.getContext()), AllocaBuilder(M.getContext()),
  currentFunction(nullptr)
{
}	

Codegeneration::IRCreator::~IRCreator()
{
  verifyModule(M);
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
  auto function = llvm::Function::Create(
      function_type,
      llvm::GlobalValue::ExternalLinkage,
      name,
      &M
      );
  if (!definition) {
    return function;
  }
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

void Codegeneration::IRCreator::makeConditonalBranch(
    llvm::Value* branchCondition,
    llvm::BasicBlock* consequenceBlock,
    llvm::BasicBlock* alternativeBlock)
{
  branchCondition = Builder.CreateSExt(branchCondition, Builder.getInt32Ty());
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

void Codegeneration::IRCreator::connect(llvm::BasicBlock* from, llvm::BasicBlock* to)
{
  if (!from) {
    from = Builder.GetInsertBlock();
  }
  llvm::BranchInst::Create(to, from);
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
 */
BINCREATE(createAdd) {
        lhs = Builder.CreateSExt(lhs, Builder.getInt32Ty());
        rhs = Builder.CreateSExt(rhs, Builder.getInt32Ty());
	return Builder.CreateAdd(lhs,rhs);
}

BINCREATE(createMinus) {
        lhs = Builder.CreateSExt(lhs, Builder.getInt32Ty());
        rhs = Builder.CreateSExt(rhs, Builder.getInt32Ty());
	return Builder.CreateSub(lhs, rhs);
}

/*
 * We use signed less than for the less than operator as usual arithmetic conver
 * sions imply an implicit threatment of all values as i32 integer which are
 * signed in our C subset
 */
BINCREATE(createLess) {
	lhs = Builder.CreateSExt(lhs, Builder.getInt32Ty());
        rhs = Builder.CreateSExt(rhs, Builder.getInt32Ty());
        return Builder.CreateICmpSLT(lhs,rhs);
}

BINCREATE(createMult) {
        lhs = Builder.CreateSExt(lhs, Builder.getInt32Ty());
        rhs = Builder.CreateSExt(rhs, Builder.getInt32Ty());
	return Builder.CreateMul(lhs, rhs);
}

BINCREATE(createUnequal){
        lhs = Builder.CreateSExt(lhs, Builder.getInt32Ty());
        rhs = Builder.CreateSExt(rhs, Builder.getInt32Ty());
	return Builder.CreateICmpNE(lhs,rhs);
}

BINCREATE(createEqual){
        lhs = Builder.CreateSExt(lhs, Builder.getInt32Ty());
        rhs = Builder.CreateSExt(rhs, Builder.getInt32Ty());
	return Builder.CreateICmpEQ(lhs,rhs);
}

BINCREATE(createLogAnd){
        lhs = Builder.CreateSExt(lhs, Builder.getInt32Ty());
        rhs = Builder.CreateSExt(rhs, Builder.getInt32Ty());
        return Builder.CreateAnd(lhs, rhs);
}

BINCREATE(createLogOr){
        lhs = Builder.CreateSExt(lhs, Builder.getInt32Ty());
        rhs = Builder.CreateSExt(rhs, Builder.getInt32Ty());
	return Builder.CreateOr(lhs,rhs);
}

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

BINCREATEL(createAccess) {
        UNUSED(rhs);
        std::vector<llvm::Value *> indexes;
        indexes.push_back(Builder.getInt32(0));
        indexes.push_back(Builder.getInt32(index));
        //Compute the GEP
        llvm::Value *GEP = Builder.CreateInBoundsGEP(lhs, indexes);
        llvm::LoadInst *var = Builder.CreateLoad(GEP);
        return var;
}

llvm::Value* Codegeneration::IRCreator::createAssign(llvm::Value* lhs, 
                llvm::Value* rhs, llvm::Type* type) {
  rhs = Builder.CreateSExtOrTrunc(rhs, type);
  store(rhs,lhs);
  return lhs;
}

BINCREATEL(getAddressfromPointer){
        UNUSED(rhs);
        std::vector<llvm::Value *> indexes;
        indexes.push_back(Builder.getInt32(0));
        indexes.push_back(Builder.getInt32(index));
        //Compute the GEP & return it
        llvm::Value *GEP = Builder.CreateInBoundsGEP(lhs, indexes);
        return GEP;
}

BINCREATEL(getMemberAddress){
        UNUSED(rhs);
        std::vector<llvm::Value *> indexes;
        indexes.push_back(Builder.getInt32(0));
        indexes.push_back(Builder.getInt32(index));
        //Compute the GEP & return it
        llvm::Value *GEP = Builder.CreateInBoundsGEP(lhs, indexes);
        return GEP;
}

BINCREATEL(getArrayPosition) {
        UNUSED(index);
        lhs = Builder.CreateSExt(lhs, Builder.getInt32Ty());
        rhs = Builder.CreateSExt(rhs, Builder.getInt32Ty());
	return Builder.CreateAdd(lhs,rhs);
}

UNCREATE(createLogNeg) {
        return Builder.CreateNot(val);
}

UNCREATE(createNeg) { 
        return Builder.CreateNeg(val);
}

UNCREATE(createDeref) { 
        return Builder.CreateLoad(val);
}


UNCREATE(createAddress) {
        std::vector<llvm::Value*> indexes ;
        indexes.push_back(Builder.getInt32(0));
        return Builder.CreateGEP(val,indexes);
}

llvm::Value* Codegeneration::IRCreator::createSizeof(llvm::Type* type)
{
  return llvm::ConstantExpr::getSizeOf(type);
}

UNCREATE(getDeref) {
        return Builder.CreateLoad(val);
}

UNCREATE(getAddress) {
        std::vector<llvm::Value*> indexes ;
        indexes.push_back(Builder.getInt32(0));
        return Builder.CreateGEP(val,indexes);}

llvm::Value* Codegeneration::IRCreator::loadVariable(
                llvm::Value *val) {
  return Builder.CreateLoad(val);
}

ALLOCF(allocLiteral) {
        return Builder.CreateGlobalString(llvm::StringRef(name));
}

llvm::Value* Codegeneration::IRCreator::allocChar (char val) {
  return Builder.getInt8(val);
}

llvm::Value* Codegeneration::IRCreator::allocInt(int val){
  return Builder.getInt32(val);
}

ALLOCF(allocNullptr) {
  UNUSED(name);
  return Builder.getInt32(0); //FIXME
}

llvm::Value* Codegeneration::IRCreator::makeSelect(Parsing::SubExpression cond,
                Parsing::SubExpression lhs, Parsing::SubExpression rhs) {
        llvm::BasicBlock* left = getControlFlowBlock();
        llvm::BasicBlock* right = getControlFlowBlock();
        llvm::BasicBlock* term = getControlFlowBlock();
        llvm::Value* branch = cond->emit_rvalue(this);
        llvm::Instruction* inst = Builder.CreateCondBr(branch, left, right);
        Builder.Insert(inst);
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
        llvm::Instruction* inst = Builder.CreateCondBr(branch, left, right);
        Builder.Insert(inst);
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


llvm::Value* Codegeneration::IRCreator::createFCall(llvm::Value* func,
                std::vector<llvm::Value*> params) { 
  return Builder.CreateCall(func, params);
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
  std::cout<<"set point"<<std::endl;
  currentBreakPoint = block;
}


void Codegeneration::IRCreator::makeBreak() {
  connect(currentBreakPoint);
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
        llvm_type = llvm::PointerType::getUnqual(
            semantic_type2llvm_type(pointee)
            );
      break;
      }
                                 
    case Semantic::Type::STRUCT:
      {
        auto structType =
          std::static_pointer_cast<Parsing::StructDeclaration>(semantic_type);
        if (structType->llvm_type) {
          llvm_type = structType->llvm_type;
          break;
        }
        auto struct_type = llvm::StructType::create(
            this->Builder.getContext(),
            structType->toString()
            );
        structType->llvm_type = struct_type;
        /* TODO: handle non primitive types*/
        std::vector<llvm::Type *> member_types;
        // TODO: use transform
        for (auto member: structType->members()) {
          member_types.push_back(semantic_type2llvm_type(member.second));
        }
        struct_type->setBody(member_types);
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
        parameter_types.push_back(this->semantic_type2llvm_type(p));
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

int Codegeneration::IRCreator::computeIndex (Parsing::SubExpression lhs, Parsing::SubExpression rhs){
  Parsing::SemanticDeclarationNode type = lhs->getType();
  auto  structtype = std::static_pointer_cast<Parsing::StructDeclaration> (type);
  auto it = structtype->members().begin();
  //our indexes start with 0 as in the example file
  int i = 0;
  //now compute the index with the compareTypes function
  while (! Semantic::compareTypes(it->second, rhs->getType())){
    ++it;
    ++i;
  }
  return i;
}
