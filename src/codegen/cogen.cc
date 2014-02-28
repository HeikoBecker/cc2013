#include "cogen.h"
#include "../parser/ast.h"
#include "../parser/astNode.h"
#include "../parser/statementNode.h"
#include "../parser/expressionNode.h"
#include "../parser/semantic.h"
#include "../utils/exception.h"
#include "../utils/util.h"
#include "labelgen.h"

#include <memory>
#include <algorithm>

#include "llvm/IR/Module.h"                /* Module */
#include "llvm/IR/Function.h"              /* Function */
#include "llvm/IR/Constant.h"              /* Constant::getNullValue */
#include "llvm/IR/IRBuilder.h"             /* IRBuilder */
#include "llvm/IR/LLVMContext.h"           /* LLVMContext */
#include "llvm/IR/GlobalValue.h"           /* GlobaleVariable, LinkageTypes */
#include "llvm/Analysis/Verifier.h"        /* verifyFunction, verifyModule */
#include "llvm/Support/raw_ostream.h"

//convenience macros to save some typing time
#define EMIT_IR(X) void X::emitIR(Codegeneration::IRCreator* creator)
#define EMIT_LV(X) llvm::Value* X::emit_lvalue(Codegeneration::IRCreator* creator) 
#define EMIT_RV(X) llvm::Value* X::emit_rvalue(Codegeneration::IRCreator* creator) 


Codegeneration::IRCreator::IRCreator(llvm::Module* M, llvm::IRBuilder<>* Builder,
					llvm::IRBuilder<>* AllocaBuilder):
M(M), Builder(Builder), AllocaBuilder(AllocaBuilder) {}	

Codegeneration::IRCreator::~IRCreator(){}


/*
 *  Converts one of our type classes to the corresponding LLVM Type
 */
llvm::Type* Codegeneration::IRCreator::semantic_type2llvm_type(
    const Parsing::SemanticDeclarationNode semantic_type) {
  llvm::Type *llvm_type = nullptr;
  switch(semantic_type->type()){
    case Semantic::Type::INT:
      llvm_type = Builder->getInt32Ty();
      break;

    case Semantic::Type::CHAR:
      llvm_type = Builder->getInt8Ty();
      break;
                              
    case Semantic::Type::VOID:
      llvm_type = Builder->getVoidTy();
      break;
                              
    case Semantic::Type::ARRAY:
    case Semantic::Type::POINTER:
      {
        auto pointer_type =
          std::static_pointer_cast<Parsing::PointerDeclaration>(semantic_type);	
        llvm_type = llvm::PointerType::getUnqual(
            semantic_type2llvm_type(pointer_type->pointee())
            );
      break;
      }
                                 
    case Semantic::Type::STRUCT:
      {
        auto structType =
          std::static_pointer_cast<Parsing::StructDeclaration>(semantic_type);
        UNUSED(structType);
        llvm_type = llvm::StructType::create(this->Builder->getContext());
        /* TODO: handle non primitive types*/
        std::vector<llvm::Type *> member_types;
        // TODO: use transform
        for (auto member: structType->members()) {
          member_types.push_back(semantic_type2llvm_type(member.second));
        }
      }
    case Semantic::Type::FUNCTION:
      // Should we handle functions here?
    default:
      llvm_type = Builder->getInt32Ty();
  }
  return llvm_type;
}


void Codegeneration::genLLVMIR(const char* filename, Parsing::AstRoot root) {

  auto &Ctx = llvm::getGlobalContext();
  std::string errorStr;
  llvm::raw_fd_ostream stream(filename, errorStr);
  llvm::Module* M = new llvm::Module(filename, Ctx);
  llvm::IRBuilder<>* Builder = new llvm::IRBuilder<>(Ctx);
  llvm::IRBuilder<>* AllocaBuilder = new llvm::IRBuilder<>(Ctx);
  Codegeneration::IRCreator Creator (M, Builder, AllocaBuilder);
  root->emitIR(&Creator);
  verifyModule(*M);
  (*M).print(stream, nullptr); /* M is a llvm::Module */
}

EMIT_IR(Parsing::AstNode)
{
  UNUSED(creator); //FIXME
}

EMIT_IR(Parsing::TranslationUnit)
{
  // call emitIR on each external declaration of the translation unit
  for (auto external_declaration: this->externalDeclarations) {
    external_declaration->emitIR(creator);
  }
}

EMIT_IR(Parsing::ExternalDeclaration)
{
  //FIXME: use IRcreator
  // TODO: type mapping needs to go into a different method!
  using namespace llvm;
  // we either have to work with a global declaration or a forward declaration
  // function definitions are handled in FunctionDefinition
  // first we take the type of the node

  llvm::Type * external_declaration_type = creator->semantic_type2llvm_type(
      this->getSemanticNode()
  );
  
  // TODO: move global variable creato into creator method?
  GlobalVariable *GlobVar = new GlobalVariable(
          *creator->M                                      /* Module & */,
          external_declaration_type                              /* Type * */,
          false                                   /* bool isConstant */,
          GlobalValue::CommonLinkage              /* LinkageType */,
          llvm::Constant::getNullValue(external_declaration_type)      /* Constant * Initializer */,
          "TODO"                                /* const Twine &Name = "" */,
          /*--------- We do not need this part (=> use defaults) ----------*/
          0                                       /* GlobalVariable *InsertBefore = 0 */,
          GlobalVariable::NotThreadLocal          /* ThreadLocalMode TLMode = NotThreadLocal */,
          0                                       /* unsigned AddressSpace = 0 */,
          false                                   /* bool isExternallyInitialized = false */);
   //TODO: what should we do with the global variable now?
  std::cout << this->declarator->getIdentifier();
  GlobVar->setName(this->declarator->getIdentifier()); // FIXME: we probably want a get name method
}

EMIT_IR(Parsing::FunctionDefinition)
{
  // TODO: make name a member of functiondefiniton or declaration
  auto name = this->declarator->getIdentifier();
  // lookup the type of the current function
  auto semtree = Parsing::SemanticForest::filename2SemanticTree(this->pos().name);
  auto function_type_ = std::static_pointer_cast<FunctionDeclaration>(semtree->lookUpType(name, this->pos()));
  // lookup the return type and set it correctly
  auto return_type_ = function_type_->returnType();
  /* TODO: set the correct return type */
  auto return_type = nullptr; //FIXME
  /*************************************/
  /* TODO: set the correct parameter types */
  auto parameter_types = std::vector<llvm::Type *>(function_type_->parameter().size());
  // iterate over parameter_types and push corresponding LLVM type into vector
  std::transform(
      function_type_->parameter().cbegin(),
      function_type_->parameter().cend(),
      begin(parameter_types),
      [&](SemanticDeclarationNode s) {
        //return semantic_type2llvm_type(Builder, s); // FIXME: use IRCreator
        UNUSED(s);
        return nullptr;
  });
  /*************************************/
  llvm::FunctionType* function_type = llvm::FunctionType::get(
      return_type,
      parameter_types,
      /*isVarArg=*/false); 
  auto function = llvm::Function::Create(
      function_type,
      llvm::GlobalValue::ExternalLinkage,
      name,
      nullptr // FIXME
      );
  /* TODO: set the names of the function arguments
   * byiterating over them and calling setName*/
  // TODO: retrive function argument names
  auto argument_it = function->begin();
  UNUSED(argument_it);
  /********************************************/
  // Create the basic block for the function
  auto function_basic_block = llvm::BasicBlock::Create(
      creator->M->getContext(), // FIXME: M
      name + "entry",
      function,
      0 //InsertBefore: inserts at end of surrounding function?
      );
  //Builder.SetInsertPoint(function_basic_block); // FIXME: IRCreator now needs
  //function for this
  /* TODO: store each argument on the stack
   * 1. Allocate a stack slot
   */
  /*
   * 2. Store the argument value
   */
  // emit code for the body
  this->compoundStatement->emitIR(creator);

  // stol^H^H^H^H borrowed from Johannes' example
  /* All code was emitted,.. but the last block might be empty.
   * If the last block does not end with a terminator statement the simple
   * rules created either dead code or the function is a void function without
   * a return on each path. Either way we need to add a terminator instruction
   * to the last block. The idea is to look at the return type of the current
   * function and emit either a void return or a return with the 'NULL' value
   * for this type */
  UNUSED(function_basic_block); // reenable code  below
  //if (Builder.GetInsertBlock()->getTerminator() == nullptr) {
    //auto CurFuncReturnType = Builder.getCurrentFunctionReturnType();
    //if (CurFuncReturnType->isVoidTy()) {
      //Builder.CreateRetVoid();
    //} else {
      //Builder.CreateRet(llvm::Constant::getNullValue(CurFuncReturnType));
    //}
  //}
}

EMIT_IR(Parsing::CompoundStatement)
{
  for (auto statement : this->subStatements) {
    statement->emitIR(creator);
  }
}

//##############################################################################
//#                    Expression Code Generation                              #
//##############################################################################

/*
 * Every expression has the emitIR function that redirects the call to 
 * emit_rvalue.
 */
EMIT_IR(Parsing::ExpressionStatement)
{
  this->expression->emitIR(creator);
}

/*
 * An expression can be part of a statement with e; where e is a statement.
 * So we need! emitIR to produce the rvalue.
 * The lvalue function will be overwritten by the corresponding class so we can
 * just call it here
 * FIXME: Mark this function for inlining as it is just a forwarding/wrapper of
 * a call to emit_rvalue
 */
EMIT_IR(Parsing::Expression)
{
	this->emit_lvalue(creator);
}

/*
 * Methods for abstract expression object. They should never be called. If this
 * happens, we forgot to override the method at a specific place, that will be
 * printed by the exception.
 * FIXME: Mark for inlining
 */
EMIT_RV(Parsing::Expression) {
  UNUSED(creator);
  throw CompilerException("You did not override the method emit_rvalue", this->pos());
}
EMIT_LV(Parsing::Expression) {
  UNUSED(creator);
  throw CompilerException("You did not override the method emit_lvalue", this->pos());
}

/*
 * Creates the RVALUE of the BinaryExpression object and returns it for further
 * usage. First compute left and right values, then emit the instruction based 
 * on the operand
 */
EMIT_RV(Parsing::BinaryExpression) {
  //First compute the values for the subexpressions
  llvm::Value* lhs = this->lhs->emit_rvalue(creator);
  llvm::Value* rhs = this->rhs->emit_rvalue(creator);

  UNUSED(lhs);
  UNUSED(rhs);

  //then compute the corresponding value based on the operand
  switch(this->op){
	case PunctuatorType::PLUS:
		break;
	case PunctuatorType::MINUS:
		break;
	case PunctuatorType::LESS:
		break;
	case PunctuatorType::STAR:
		break;
	case PunctuatorType::NEQUAL:
		break;
	case PunctuatorType::EQUAL:
		break;
	case PunctuatorType::LAND:
		break;
	case PunctuatorType::LOR:
		break;
	case PunctuatorType::ARROW:
		break;
	case PunctuatorType::MEMBER_ACCESS:
		break;
	case PunctuatorType::ASSIGN:
		break;
	default:
	  throw CompilerException("INTERNAL ERROR", this->pos());
}
  
  return nullptr;
}

/*
 * Produces the corresponding rvalue. First the rvalue of the operand is 
 * computed, then the operator is applied.
 */
EMIT_RV(Parsing::UnaryExpression) {
  UNUSED(creator); //FIXME
  return nullptr;
}

/*
 * A unary operator can be a valid lvalue, so we need to allow this for code 
 * generation. //TODO: Determine correct operators or not?
 * First compute the lvalue of the operand and then apply the operator.
 * Corresponding value is returned.
 */
EMIT_LV(Parsing::UnaryExpression) {
  UNUSED(creator); //FIXME
  return nullptr;
}

/* 
 * Produces the rvalue of a variable by returning the variable name that llvm 
 * gave it so that we can do computations with it
 */
EMIT_RV(Parsing::VariableUsage) {
  UNUSED(creator); //FIXME
  return nullptr;
}

/*
 * Computes a variables lvalue by returning the variable, that llvm has 
 * produced when it has been declared.
 */
EMIT_LV(Parsing::VariableUsage) {
  UNUSED(creator); //FIXME
  return nullptr;
}

/*
 * Produces the rvalue of a literal by returning its variable.
 */
EMIT_RV(Parsing::Literal) {
  UNUSED(creator); //FIXME
  return nullptr;
}

/*
 * Produces the lvalue of a literal, as string literals are arrays and therefore
 * they have an address where they are stored.
 */
EMIT_LV(Parsing::Literal) {
  UNUSED(creator); //FIXME
  return nullptr;
}

/*
 * Produces the constants value. It has been validated befory by the semantics
 * so we can advise llvm to create a new constant if it does not exist and 
 * return the value
 */
EMIT_RV(Parsing::Constant) {
  UNUSED(creator); //FIXME
  return nullptr;
}

/*
 * A function call can produce a valid value. So we need to evaluate all 
 * parameters and then create the corresponding function call.
 */
EMIT_RV(Parsing::FunctionCall) {
  UNUSED(creator); //FIXME
  return nullptr;
}

/*
 * A function can return the address of some valid space so it can also produce
 * a valid rvalue. this has been validated by the semantics. We only need
 * to compute the value and then return it.
 */
EMIT_LV(Parsing::FunctionCall) {
 UNUSED(creator); //FIXME
 return nullptr;
}

/*
 * A ternary operator can produce a valid rvalue. First evaluate the condition.
 * Then return the value based on the condition.
 * TODO: Use the phi trick that was shown in the slides!
 */
EMIT_RV(Parsing::TernaryExpression) {
  UNUSED(creator); //FIXME
  return nullptr;
}

/*
 * Produce the ternary expressions value. Remember that both expressions need to
 * be evaluated as one could contain side effects.
 * TODO: Use the phi trick that was shown in the slides!
 */
EMIT_LV(Parsing::TernaryExpression) {
 UNUSED(creator); //FIXME
 return nullptr;
}

/*
 * Produces the rvalue of the sizeof expression. TODO!
 */
EMIT_RV(Parsing::SizeOfExpression) {
  UNUSED(creator); //FIXME
  return nullptr;
}
