#include <memory>

#include <Ast.h>
#include <Lexer.h>

namespace ppc {
namespace Parsing {

using std::shared_ptr;
using std::make_shared;

void Block::addStatement(shared_ptr<AstNode> node) {
    node->parent = this->get_shared_ptr();
    this->statements.push_back(node);
    this->setNewLineAndSpaced(node);
}

Block::Block(shared_ptr<AstNode> stmt) {
    this->statements.push_back(stmt);
    this->column = stmt->column;
}

Block::Block(shared_ptr<Token> firstTok) {
    this->column = firstTok->column;
}

void Block::initParent() {
    if (!this->statements.empty()) {
        this->statements[0]->parent = this->get_shared_ptr();
    }
}

void TranslationUnit::initParent() { }

TranslationUnit::TranslationUnit(shared_ptr<Token> tok) : Block(tok) { }

//
// Statement
//
ReturnStmt::ReturnStmt(shared_ptr<Token> returnTok,
shared_ptr<Expression> returnExpr) :
returnExpr(returnExpr)
{
    this->column = returnTok->column;
    if (returnExpr) {
        this->setNewLineAndSpaced(returnExpr);
    } else {
        this->setNewLineAndSpaced(returnTok);
    }
}

void ReturnStmt::initParent() {
    if (this->returnExpr) {
        this->returnExpr->parent = this->get_shared_ptr();
    }
}

SwitchLabel::SwitchLabel(shared_ptr<Token> labelTok) :
isCase(labelTok->value == "case"),
isWhen(labelTok->value == "when"),
isDefault(labelTok->value == "default")
{
    this->column = labelTok->column;
}

void SwitchLabel::initParent() { }

SwitchStmt::SwitchStmt(shared_ptr<Token> switchTok,
                       shared_ptr<Expression> operand) :
minIndent(0),
operand(operand)
{
    this->column = switchTok->column;
}

void SwitchStmt::initParent() {
    this->operand->parent = this->get_shared_ptr();
}

void SwitchStmt::addLabel(shared_ptr<SwitchLabel> label) {
    this->labelList.push_back(label);
    this->setNewLineAndSpaced(label);
    label->parent = this->get_shared_ptr();
}

ForStmt::ForStmt(shared_ptr<Block> block,
        shared_ptr<Expression> condExpr,
        shared_ptr<Expression> stepExpr) :
block(block),
condExpr(condExpr),
stepExpr(stepExpr)
{ }

void ForStmt::initParent() {
    this->block->parent = this->get_shared_ptr();
    if (this->condExpr) {
        this->condExpr->parent = this->get_shared_ptr();
    }
}

ForInStmt::ForInStmt(shared_ptr<Token> forTok,
                     shared_ptr<Identifier> valueVar,
                     shared_ptr<Identifier> indexVar,
                     shared_ptr<Expression> rangeExpr,
                     shared_ptr<Token> doTok,
                     shared_ptr<Block> block,
                     shared_ptr<Expression> condExpr,
                     shared_ptr<Expression> stepExpr) :
valueVar(valueVar),
indexVar(indexVar),
rangeExpr(rangeExpr),
ForStmt(block, condExpr, stepExpr)
{
    this->column = forTok->column;
    this->singleLine = static_cast<bool>(doTok);
    this->setNewLineAndSpaced(block);
}

void ForInStmt::initParent() {
    ForStmt::initParent();

    this->valueVar->parent = this->rangeExpr->parent = this->get_shared_ptr();
    if (this->indexVar) this->indexVar->parent = this->get_shared_ptr();
}

ForFromStmt::ForFromStmt(shared_ptr<Token> forTok,
                         shared_ptr<Expression> itExpr,
                         shared_ptr<Expression> floorExpr,
                         shared_ptr<Expression> upperExpr,
                         shared_ptr<Token> doTok,
                         shared_ptr<Block> block,
                         shared_ptr<Expression> condExpr,
                         shared_ptr<Expression> stepExpr) :
itExpr(itExpr),
floorExpr(floorExpr),
upperExpr(upperExpr),
ForStmt(block, condExpr, stepExpr)
{
    this->column = forTok->column;
    this->singleLine = static_cast<bool>(doTok);
    this->setNewLineAndSpaced(block);
}

void ForFromStmt::initParent() {
    this->itExpr->parent
        = this->floorExpr->parent
        = this->upperExpr->parent
        = this->block->parent
        = this->get_shared_ptr();
}
LoopStmt::LoopStmt(shared_ptr<Token> loopTok,
                   shared_ptr<Block> block) :
block(block)
{
    this->column = loopTok->column;
    this->setNewLineAndSpaced(block);
}

void LoopStmt::initParent() {
    this->block->parent = this->get_shared_ptr();
}

DoWhileStmt::DoWhileStmt(shared_ptr<Token> doTok,
                         shared_ptr<Block> block,
                         shared_ptr<Expression> cond) :
singleLine(!doTok->newLine),
block(block),
condExpr(cond)
{
    this->column = doTok->column;
    this->setNewLineAndSpaced(cond);
}

void DoWhileStmt::initParent() {
    this->block->parent = this->condExpr->parent = this->get_shared_ptr();
}

WhileStmt::WhileStmt(shared_ptr<Token> whileTok,
                     shared_ptr<Expression> cond,
                     shared_ptr<Block> block) :
condExpr(cond),
block(block)
{
    this->column = whileTok->column;
    this->setNewLineAndSpaced(this->block);
}

void WhileStmt::initParent() {
    this->condExpr->parent = this->block->parent = this->get_shared_ptr();
}

OnScopeExit::OnScopeExit(shared_ptr<Token> oseTok,
                         shared_ptr<Block> block) :
block(block)
{
    this->column = oseTok->column;
    this->setNewLineAndSpaced(block);
}

void OnScopeExit::initParent() {
    this->block->parent = this->get_shared_ptr();
}

Literal::Literal(shared_ptr<Token> tok) : token(tok) {
    this->column = tok->column;
    this->setNewLineAndSpaced(this->token);
}

void Literal::initParent() { }

void CompoundLiteral::addKvPair(shared_ptr<kvPair> pair) {
    this->initializerList.push_back(*pair);
    if (!this->minIndent) {
        if (pair->first) {
            this->minIndent = pair->first->column;
        } else{
            this->minIndent = pair->second->column;
        }
    }

    if (pair->first) {
        pair->first->parent = this->get_shared_ptr();
    }

    if (pair->second) {
        pair->second->parent = this->get_shared_ptr();
    }

    this->setNewLineAndSpaced(pair->second);
}

CompoundLiteral::CompoundLiteral(shared_ptr<Token> left) :
isArray(left && left->type == tokType::L_BRACKET),
isStruct(left && left->type == tokType::L_BRACE),
isImplicit(!static_cast<bool>(left))
{
    this->column = left->column;
}

CompoundLiteral::CompoundLiteral() :
isArray(false),
isStruct(false),
isImplicit(true)
{ }

void CompoundLiteral::initParent() { }

FunctionDefinition::
FunctionDefinition(shared_ptr<Token> inlineTok,
                   shared_ptr<Token> noReturnTok,
                   shared_ptr<TypeSpecifier> returnType,
                   shared_ptr<Identifier> functionName,
                   std::vector<shared_ptr<FunctionParamDecl>> &paramDeclList,
                   bool hasVariableArg,
                   shared_ptr<Block> functionBody) :
isInline(static_cast<bool>(inlineTok)),
isNoReturn(static_cast<bool>(noReturnTok)),
returnType(returnType),
functionName(functionName),
paramDeclList(paramDeclList),
hasVariableArg(hasVariableArg),
functionBody(functionBody)
{
    if (inlineTok && inlineTok->prev != noReturnTok) {
        this->column = inlineTok->column;
    } else if (noReturnTok) {
        this->column = noReturnTok->column;
    } else {
        this->column = this->returnType->column;
    }

    this->setNewLineAndSpaced(functionBody);
}

void FunctionDefinition::initParent() {
    this->returnType->parent
        = this->functionBody->parent
        = this->get_shared_ptr();

    if (this->functionName) {
        this->functionName->parent = this->get_shared_ptr();
    }

    for (auto param : this->paramDeclList) {
        param->parent = this->get_shared_ptr();
    }
}

UnaryArithmeticExpression::
UnaryArithmeticExpression(shared_ptr<Token> optr,
                          shared_ptr<Expression> rightExpr):
optr(optr),
operand(rightExpr),
postfix(false)
{
    this->column = this->optr->column;
    this->setNewLineAndSpaced(rightExpr);
}

UnaryArithmeticExpression::
UnaryArithmeticExpression(shared_ptr<Expression> leftExpr,
                          shared_ptr<Token> optr):
optr(optr),
operand(leftExpr),
postfix(true)
{
    this->column = this->operand->column;
    this->setNewLineAndSpaced(optr);
}

void UnaryArithmeticExpression::initParent() {
    this->operand->parent = this->get_shared_ptr();
}

BinaryExpression::BinaryExpression(shared_ptr<Expression> left,
                                   shared_ptr<Token> optr,
                                   shared_ptr<Expression> right):
left(left),
optr(optr),
right(right) {
    this->column = left->column;
    this->setNewLineAndSpaced(right);
}

void BinaryExpression::initParent() {
    this->left->parent = this->right->parent = this->get_shared_ptr();
}

BraceExpression::BraceExpression(shared_ptr<Token> lParen,
                                 shared_ptr<Expression> expr,
                                 shared_ptr<Token> RParen):
expr(expr)
{
    this->column = lParen->column;
    this->setNewLineAndSpaced(RParen);
}

void BraceExpression::initParent() {
    this->expr->parent = this->get_shared_ptr();
}


IfExpression::IfExpression(shared_ptr<Token> ifTok,
                           shared_ptr<Token> unlikelyTok,
                           shared_ptr<Expression> condExpr,
                           shared_ptr<Block> thenBlock,
                           shared_ptr<Block> elseBlock):
cond(condExpr),
thenBlock(thenBlock),
isUnlikely(static_cast<bool>(unlikelyTok)),
elseBlock(elseBlock)
{
    this->column = ifTok->column;
    if (this->elseBlock) {
        this->setNewLineAndSpaced(elseBlock);
    } else {
        this->setNewLineAndSpaced(thenBlock);
    }
}

void IfExpression::initParent() {
    this->cond->parent = this->thenBlock->parent = this->get_shared_ptr();
    if (this->elseBlock) {
        this->elseBlock->parent = this->get_shared_ptr();
    }
}


LongjmpExpression::LongjmpExpression(shared_ptr<Token> longjmpTok,
                                     shared_ptr<Expression> jmpBuf,
                                     shared_ptr<Expression> intVal,
                                     shared_ptr<Token> rParen) :
jmpBuf(jmpBuf),
intVal(intVal)
{
    this->column = longjmpTok->column;
    if (rParen) {
        this->setNewLineAndSpaced(rParen);
    } else {
        this->setNewLineAndSpaced(intVal);
    }
}

void LongjmpExpression::initParent() {
    this->jmpBuf->parent = this->intVal->parent = this->get_shared_ptr();
}


VaArgExpression::VaArgExpression(shared_ptr<Token> VaArgTok,
                                 shared_ptr<Expression> argumentExpr,
                                 shared_ptr<TypeSpecifier> typeSpecifier,
                                 shared_ptr<Token> rParen) :
argumentExpr(argumentExpr),
typeSpecifier(typeSpecifier)
{
    this->column = VaArgTok->column;

    if (rParen) {
        this->setNewLineAndSpaced(rParen);
    } else {
        this->setNewLineAndSpaced(typeSpecifier);
    }
}

void VaArgExpression::initParent() {
    this->argumentExpr->parent
        = this->typeSpecifier->parent
        = this->get_shared_ptr();
}

OffsetOfExpression::OffsetOfExpression(shared_ptr<Token> offsetOfTok,
                   shared_ptr<TypeSpecifier> typeSpecifier,
                   shared_ptr<Token> fieldName,
                   shared_ptr<Token> rParen) :
typeSpecifier(typeSpecifier),
fieldName(fieldName)
{
    this->column = offsetOfTok->column;
    this->setNewLineAndSpaced(rParen ? rParen : fieldName);
}

void OffsetOfExpression::initParent() {
    this->typeSpecifier->parent = this->get_shared_ptr();
}

SizeOfExpression::SizeOfExpression(shared_ptr<Token> sizeofTok,
                                   shared_ptr<Expression> right):
operand(right),
isExpr(true),
isTypeSpecifier(false)
{
    this->column = sizeofTok->column;
    this->setNewLineAndSpaced(right);
}

SizeOfExpression::SizeOfExpression(shared_ptr<Token> sizeofTok,
shared_ptr<TypeSpecifier> right):
operand(right),
isExpr(false),
isTypeSpecifier(true)
{
    this->column = sizeofTok->column;
    this->setNewLineAndSpaced(right);
}

void SizeOfExpression::initParent() {
    this->operand->parent = this->get_shared_ptr();
}

AlignOfExpression::AlignOfExpression(shared_ptr<Token> alignofTok,
                                     shared_ptr<TypeSpecifier> typeSpecifier):
operand(typeSpecifier)
{
    this->column = alignofTok->column;
    this->setNewLineAndSpaced(typeSpecifier);
}

void AlignOfExpression::initParent() {
    this->operand->parent = this->get_shared_ptr();
}

ArrayAccess::ArrayAccess(shared_ptr<Expression> array,
                         shared_ptr<Expression> index,
                         shared_ptr<Token> rBracket) :
arrayExpr(array),
subscript(index)
{
    this->column = arrayExpr->column;
    this->setNewLineAndSpaced(rBracket);
}

void ArrayAccess::initParent() {
    this->arrayExpr->parent = this->subscript->parent = this->get_shared_ptr();
}

StructOrUnionFieldAccess::
StructOrUnionFieldAccess(shared_ptr<Expression> obj,
                         shared_ptr<Token> query,
                         shared_ptr<Token> periodOrArrow,
                         shared_ptr<Identifier> field) :
hasExistTest(static_cast<bool>(query)),
obj(obj),
isArrowAccess(periodOrArrow->type == tokType::ARROW),
field(field)
{
    this->setNewLineAndSpaced(field);
}

StructOrUnionFieldAccess::
StructOrUnionFieldAccess(shared_ptr<Expression> obj,
                         shared_ptr<Token> query,
                         shared_ptr<Token> periodOrArrow,
                         shared_ptr<Token> id):
StructOrUnionFieldAccess(obj,
                         query,
                         periodOrArrow,
                         Identifier::init(id))
{ }

void StructOrUnionFieldAccess::initParent() {
    this->obj->parent = this->field->parent = this->get_shared_ptr();
}

CastExpression::CastExpression(shared_ptr<Token> lParen,
                               shared_ptr<TypeSpecifier> specifier,
                               shared_ptr<Expression> expr) :
typeSpecifier(specifier),
operand(expr)
{
    this->column = expr->column;
    this->setNewLineAndSpaced(expr);
}

void CastExpression::initParent() {
    this->typeSpecifier->parent
        = this->operand->parent
        = this->get_shared_ptr();
}

void ArgumentExpressionList::initParent() { }

UndeterminedCall::UndeterminedCall(shared_ptr<Expression> caller,
                                   shared_ptr<ArgumentExpressionList> arguments,
                                   shared_ptr<Token> rParen):
callExpr(caller),
arguments(arguments),
isImplicit(!static_cast<bool>(rParen))
{
    this->column = caller->column;

    if (rParen) {
        this->setNewLineAndSpaced(rParen);
    } else {
        this->setNewLineAndSpaced(arguments);
    }
}

void UndeterminedCall::initParent() {
    this->callExpr->parent = this->arguments->parent = this->get_shared_ptr();
}

Identifier::Identifier(shared_ptr<Token> tok) : token(tok) {
    this->column = tok->column;
    this->setNewLineAndSpaced(tok);
}

void Identifier::initParent() { }

//
// Declaration
//
BasicTypeSpecifier::BasicTypeSpecifier(shared_ptr<Token> tok) :
token(tok),
isComplex(false),
isImaginary(false)
{
    this->isBasicTypeSpecifier = true;
    this->subType = nullptr;

    this->column = token->column;
    this->setNewLineAndSpaced(tok);
}

BasicTypeSpecifier::
BasicTypeSpecifier(shared_ptr<Token> tok, shared_ptr<Token> compOrImag) :
token(tok),
isComplex(compOrImag->type == tokType::COMPLEX),
isImaginary(compOrImag->type == tokType::IMAGINARY)
{
    this->column = token->column;
    this->setNewLineAndSpaced(compOrImag);
}

void BasicTypeSpecifier::initParent() { }

TypedefSpecifier::TypedefSpecifier(shared_ptr<Token> tok) : token(tok) {
    this->column = tok->column;
}

void TypedefSpecifier::initParent() { }

PointerTypeSpecifier::PointerTypeSpecifier(shared_ptr<Token> astrisk,
                                           shared_ptr<TypeSpecifier> sub,
                                           shared_ptr<Token> rParen) :
TypeSpecifier(sub)
{
    this->column = astrisk->column;
    if (rParen) {
        this->setNewLineAndSpaced(rParen);
    } else {
        this->setNewLineAndSpaced(sub);
    }
}

void PointerTypeSpecifier::initParent() {
    this->subType->parent = this->get_shared_ptr();
}

Enumerator ::Enumerator(shared_ptr<Identifier> name,
                        shared_ptr<Expression> value) :
name(name),
value(value) {
    this->column = this->name->column;

    if (value) {
        this->setNewLineAndSpaced(value);
    } else {
        this->setNewLineAndSpaced(name);
    }
}

void Enumerator::initParent() {
    this->name->parent = this->get_shared_ptr();
    if (this->value) {
        this->value->parent = this->get_shared_ptr();
    }
}

EnumSpecifier::EnumSpecifier(shared_ptr<Token> enumTok, shared_ptr<Token> name):
EnumSpecifier(enumTok, name ? Identifier::init(name) : nullptr) { }

static void
deduceMinIndent(shared_ptr<Token> specifierTok, unsigned long &minIndent)
{
    auto prevTok = specifierTok;
    while ((prevTok = prevTok->prev)) {
        if (prevTok->type == tokType::TIMES
#define V(eType) || prevTok->type == tokType::eType
            FOREACH_STORAGE_SPECIFIER_TOKTYPE(V)
            FOREACH_TYPE_QUALIFIER_TOKTYPE(V)
#undef V
           )
        {
            minIndent = prevTok->column;
        } else {
            break;
        }
    }
}

EnumSpecifier::EnumSpecifier(shared_ptr<Token> enumTok,
                             shared_ptr<Identifier> name) :
enumToken(enumTok),
specifierName(name),
isAnonymous(!static_cast<bool>(name))
{
    this->column = this->enumToken->column;
    this->subType = nullptr;

    this->minIndent = enumTok->column;
    deduceMinIndent(enumTok, this->minIndent);

    if (!this->isAnonymous) {
        this->setNewLineAndSpaced(name);
    }
}

void EnumSpecifier::initParent() {
    if (this->specifierName) {
        this->specifierName->parent = this->get_shared_ptr();
    }
}

void EnumSpecifier::addEnumerator(shared_ptr<Enumerator> e) {
    this->enumerators.push_back(e);
    this->noEnumerator = false;

    this->setNewLineAndSpaced(e);
    e->parent = this->get_shared_ptr();
}

StructOrUnionField::StructOrUnionField(shared_ptr<Identifier> id,
                                       shared_ptr<Expression> len) :
fieldName(id),
bitLength(len),
isBitField(static_cast<bool>(len))
{
    this->column = id->column;

    if (len) {
        this->setNewLineAndSpaced(len);
    } else {
        this->setNewLineAndSpaced(id);
    }
}

StructOrUnionField::StructOrUnionField(shared_ptr<Token> id,
                                       shared_ptr<Expression> len) :
StructOrUnionField(Identifier::init(id), len) { }

void StructOrUnionField::initParent() {
    this->fieldName->parent = this->get_shared_ptr();
    if (this->bitLength) {
        this->bitLength->parent = this->get_shared_ptr();
    }
}

StructOrUnionFieldDecl::
StructOrUnionFieldDecl(bool isTypedef, shared_ptr<TypeSpecifier> specifier) :
fieldType(specifier),
isTypedef(isTypedef)
{
    this->column = specifier->column;
}

void StructOrUnionFieldDecl::initParent() {
    this->fieldType->parent = this->get_shared_ptr();
}

void StructOrUnionFieldDecl::addField(shared_ptr<StructOrUnionField> field) {
    this->fieldList.push_back(field);
    this->setNewLineAndSpaced(field);

    field->parent = this->get_shared_ptr();
}

StructOrUnionSpecifier::StructOrUnionSpecifier(shared_ptr<Token> structTok,
                                               shared_ptr<Identifier> id) :
specifierName(id),
isAnonymous(!static_cast<bool>(id)),
isStruct(structTok->type == tokType::STRUCT),
isUnion(structTok->type == tokType::UNION)
{
    this->column = structTok->column;

    this->minIndent = structTok->column;
    deduceMinIndent(structTok, this->minIndent);

    if (!this->isAnonymous) {
        this->setNewLineAndSpaced(id);
    }
}

StructOrUnionSpecifier::StructOrUnionSpecifier(shared_ptr<Token> structTok,
                                               shared_ptr<Token> id) :
StructOrUnionSpecifier(structTok, id ? Identifier::init(id): nullptr) { }

void StructOrUnionSpecifier::initParent() {
    if (this->specifierName) {
        this->specifierName->parent = this->get_shared_ptr();
    }
}

void
StructOrUnionSpecifier::addFieldDecl(shared_ptr<StructOrUnionFieldDecl> decl) {
    this->declList.push_back(decl);
    this->setNewLineAndSpaced(decl);
    decl->parent = this->get_shared_ptr();
}

ArrayTypeSpecifier::ArrayTypeSpecifier(shared_ptr<Token> lBracket,
                                       shared_ptr<Expression> size) :
arraySize(size)
{
    this->column = lBracket->column;
}

void ArrayTypeSpecifier::initParent() {
    if (this->arraySize) {
        this->arraySize->parent = this->get_shared_ptr();
    }
}

FunctionParamDecl::FunctionParamDecl(shared_ptr<TypeSpecifier> pType,
                                     shared_ptr<Identifier> paramName,
                                     shared_ptr<Expression> defaultVal) :
paramType(pType),
paramName(paramName),
defaultValue(defaultVal)
{
    this->column = this->paramType->column;

    if (this->defaultValue) {
        this->setNewLineAndSpaced(this->defaultValue);
    } else if (this->paramName) {
        this->setNewLineAndSpaced(this->paramName);
    } else {
        this->setNewLineAndSpaced(this->paramType);
    }
}

FunctionParamDecl::FunctionParamDecl(shared_ptr<TypeSpecifier> pType,
                                     shared_ptr<Token> paramName,
                                     shared_ptr<Expression> defaultVal) :
FunctionParamDecl(pType, paramName ? Identifier::init(paramName)
                                   : nullptr, defaultVal)
{ }

void FunctionParamDecl::initParent() {
    this->paramType->parent = this->get_shared_ptr();

    if (this->paramName) {
        this->paramName->parent = this->get_shared_ptr();
    }

    if (this->defaultValue) {
        this->defaultValue->parent = this->get_shared_ptr();
    }
}

FunctionTypeSpecifier::FunctionTypeSpecifier(shared_ptr<TypeSpecifier> retType)
{
    this->setSubType(retType);
    this->column = retType->column;
}

void FunctionTypeSpecifier::initParent() {
    this->subType->parent = this->get_shared_ptr();
}

void FunctionTypeSpecifier::addParam(shared_ptr<FunctionParamDecl> paramDecl) {
    this->paramDeclList.push_back(paramDecl);
    paramDecl->parent = this->get_shared_ptr();
}

DeclInit::DeclInit(shared_ptr<Identifier> id, shared_ptr<Expression> init) :
identifier(id),
initValue(init) { }

DeclInit::DeclInit(shared_ptr<Token> id, shared_ptr<Expression> init) :
DeclInit(Identifier::init(id), init) { }

void DeclInit::initParent() {
    this->identifier->parent = this->get_shared_ptr();
    if (this->initValue) {
        this->initValue->parent = this->get_shared_ptr();
    }
}

VarOrTypedefDecl::VarOrTypedefDecl(shared_ptr<Token> storageSpecifier,
                                   shared_ptr<TypeSpecifier> typeSpecifier) :
isStatic(storageSpecifier && storageSpecifier->type == tokType::STATIC),
isRegister(storageSpecifier && storageSpecifier->type == tokType::REGISTER_SPECIFIER),
isThreadLocal(storageSpecifier && storageSpecifier->type == tokType::THREAD_LOCAL),
isTypedef(storageSpecifier && storageSpecifier->type == tokType::TYPEDEF),
typeSpecifier(typeSpecifier)
{
    if (storageSpecifier) {
        this->column = storageSpecifier->column;
    } else {
        this->column = typeSpecifier->column;
    }
}

void VarOrTypedefDecl::initParent() {
    this->typeSpecifier->parent = this->get_shared_ptr();
}

void VarOrTypedefDecl::addDeclInit(shared_ptr<DeclInit> declInit) {
    this->declInitList.push_back(declInit);
    declInit->parent = this->get_shared_ptr();
}

StaticAssertDecl::StaticAssertDecl(shared_ptr<Token> saTok,
                                   shared_ptr<Expression> testExpr,
                                   shared_ptr<Expression> errExpr,
                                   shared_ptr<Token> rParen) :
testExpr(testExpr),
errExpr(errExpr)
{
    this->column = saTok->column;
    if (rParen) {
        this->setNewLineAndSpaced(rParen);
    } else {
        this->setNewLineAndSpaced(errExpr);
    }
}

void StaticAssertDecl::initParent() {
    this->testExpr->parent = this->errExpr->parent = this->get_shared_ptr();
}

void PragmaDecl::addToken(shared_ptr<Token> tok) {
    this->tokens.push_back(tok);
    this->setNewLineAndSpaced(tok);
}

PragmaDecl::PragmaDecl(shared_ptr<Token> pragmaTok) {
    this->column = pragmaTok->column;
}

void PragmaDecl::initParent() { }

Annotation::Annotation(shared_ptr<Token> atSign,
                       shared_ptr<Identifier> annoName) :
annoName(annoName)
{
    this->column = atSign->column;
    this->setNewLineAndSpaced(annoName);
}

Annotation::Annotation(shared_ptr<Token> atSign,
                       shared_ptr<Identifier> annoName,
                       shared_ptr<ArgumentExpressionList> params,
                       shared_ptr<Token> rParen) :
annoName(annoName),
params(params)
{
    this->column = atSign->column;
    if (rParen) {
        this->setNewLineAndSpaced(rParen);
    } else {
        this->setNewLineAndSpaced(params);
    }
}
Annotation::Annotation(shared_ptr<Token> atSign, shared_ptr<Token> annoName) :
Annotation(atSign, Identifier::init(annoName)) { }

Annotation::Annotation(shared_ptr<Token> atSign,
                       shared_ptr<Token> annoName,
                       shared_ptr<ArgumentExpressionList> params,
                       shared_ptr<Token> rParen) :
Annotation(atSign, Identifier::init(annoName), params, rParen) { }

void Annotation::initParent() {
    this->annoName->parent = this->get_shared_ptr();
    if (this->params) {
        this->params->parent = this->get_shared_ptr();
    }
}

PreProcessCondition::PreProcessCondition(shared_ptr<Token> directive,
                                         shared_ptr<Expression> cond,
                                         shared_ptr<AstNode> thenBlock) :
directive(directive),
cond(cond),
stmtBlock(thenBlock)
{ }

void PreProcessCondition::initParent() {
    this->stmtBlock->parent = this->cond->parent = this->get_shared_ptr();
}
} // namespace Parsing
} // namespace ppc
