#ifndef _AST_H
#define _AST_H

#include <memory>
#include <vector>
#include <functional>
#include <unordered_map>

#include <Lexer.h>

namespace ppc {
namespace Parsing {

using std::shared_ptr;
using std::make_shared;

using ppc::Token;
using ppc::tokType;

class AstVisitor;
class SymbolTable;

class Block;
class Identifier;
class OnScopeExit;
class TypeSpecifier;
class FunctionParamDecl;
class FunctionDefinition;
class StructOrUnionSpecifier;

class AstNode : public std::enable_shared_from_this<AstNode> {
private:
    static unsigned long nNodes;
protected:
    virtual void initParent() { };
public:
    unsigned long column, id;
    bool newLine, spaced, isExternal;
    std::weak_ptr<AstNode> parent;

    virtual void accept(AstVisitor &visitor) = 0;

    shared_ptr<AstNode> inline get_shared_ptr() {
        return this->shared_from_this();
    }

    void inline setNewLineAndSpaced(shared_ptr<Token> node) {
        this->newLine = node->newLine;
        this->spaced = node->spaced;
    }

    template <class T>
    void inline setNewLineAndSpaced(T node) {
        this->newLine = std::static_pointer_cast<AstNode>(node)->newLine;
        this->spaced = std::static_pointer_cast<AstNode>(node)->spaced;
    }

    shared_ptr<Block> inline getBlockParent() {
        shared_ptr<AstNode> blockParent = this->parent.lock();
        while (!blockParent->isBlock() && !blockParent->isTranslationUnit()) {
            if (!(blockParent = blockParent->parent.lock())) {
                return nullptr;
            }
        }

        return std::dynamic_pointer_cast<Block>(blockParent);
    }

    AstNode() {
        this->id = AstNode::nNodes++;
    }

#define V(nodeClass) inline bool is##nodeClass();
    FOREACH_AST_NODE(V)
#undef V
};

#define DEFINE_ACCEPT_AND_INIT(nodeName)\
protected:                                                            \
    virtual void initParent() override;                               \
public:                                                               \
    virtual void accept(AstVisitor &visitor) override;                \
    template<class ...T> static shared_ptr<nodeName> init(T... args) {\
        auto p = shared_ptr<nodeName>(new nodeName(args...));         \
        p->initParent();                                              \
        return p;                                                     \
    }

class Block : public AstNode {
DEFINE_ACCEPT_AND_INIT(Block)
private:
    Block(shared_ptr<AstNode> stmt);
public:
    std::vector<shared_ptr<AstNode>> statements;
    shared_ptr<SymbolTable> symbolTable = make_shared<SymbolTable>();

    void addStatement(shared_ptr<AstNode> node);
    Block (shared_ptr<Token> firstTok);
};

class TranslationUnit : public Block {
DEFINE_ACCEPT_AND_INIT(TranslationUnit)
public:
    std::unordered_map<shared_ptr<Identifier>, shared_ptr<AstNode>> exportMap;
    TranslationUnit(shared_ptr<Token> tok);
};

class Expression : public AstNode {
public:
    Expression() { }
};

//
// Statement
//

class SymbolTable {
public:
    std::unordered_map<std::string, shared_ptr<AstNode>> identifierMap;
    std::unordered_map<std::string, shared_ptr<StructOrUnionSpecifier>>
        structMap, unionMap;
    typedef
        std::unordered_multimap<std::string, shared_ptr<FunctionDefinition>>
    functionMapType;
    functionMapType functionMap;

    shared_ptr<SymbolTable> parent;
    std::vector<std::weak_ptr<SymbolTable>> children;

    SymbolTable() : parent(nullptr) { }
};

class ContinueOrBreak : public AstNode {
public:
    virtual void accept(AstVisitor &visitor) override;

    bool isContinue, isBreak;
    shared_ptr<Token> token;

    ContinueOrBreak(shared_ptr<Token> token) :
    isContinue(token->type == tokType::CONTINUE),
    isBreak(token->type == tokType::BREAK),
    token(token)
    {
        this->column = token->column;
        this->setNewLineAndSpaced(token);
    }
};

class GotoStmt : public AstNode {
public:
    virtual void accept(AstVisitor &visitor) override;

    shared_ptr<Token> labelName;
    GotoStmt(shared_ptr<Token> gotoTok,
             shared_ptr<Token> labelName) :
    labelName(labelName)
    {
        this->column = gotoTok->column;
        this->setNewLineAndSpaced(labelName);
    }
};

class GotoLabel : public AstNode {
public:
    virtual void accept(AstVisitor &visitor) override;

    shared_ptr<Token> labelName;
    GotoLabel(shared_ptr<Token> labelName,
              shared_ptr<Token> colon) :
    labelName(labelName)
    {
        this->column = labelName->column;
        this->setNewLineAndSpaced(colon);
    }
};

class ReturnStmt : public AstNode {
DEFINE_ACCEPT_AND_INIT(ReturnStmt)
private:
    ReturnStmt(shared_ptr<Token> returnTok, shared_ptr<Expression> returnExpr);
public:
    shared_ptr<Expression> returnExpr;
};

class SwitchLabel : public AstNode {
DEFINE_ACCEPT_AND_INIT(SwitchLabel)
private:
    SwitchLabel(shared_ptr<Token> labelTok);
public:
    bool isCase, isWhen, isDefault;
    std::vector<shared_ptr<Expression>> condList;
    shared_ptr<Block> action;

    void addCondition(shared_ptr<Expression> cond) {
        this->condList.push_back(cond);
        cond->parent = this->get_shared_ptr();
    }
};

class SwitchStmt : public AstNode {
DEFINE_ACCEPT_AND_INIT(SwitchStmt)
private:
    SwitchStmt(shared_ptr<Token> switchTok, shared_ptr<Expression> operand);
public:
    unsigned long minIndent;
    shared_ptr<Expression> operand;
    std::vector<shared_ptr<SwitchLabel>> labelList;
    shared_ptr<OnScopeExit> onScopeExit;

    void addLabel(shared_ptr<SwitchLabel> label);
};

class ForStmt : public AstNode {
protected:
    virtual void initParent() override;

    ForStmt(shared_ptr<Block> block,
            shared_ptr<Expression> condExpr,
            shared_ptr<Expression> stepExpr);
public:
    bool singleLine;
    shared_ptr<Block> block;
    shared_ptr<Expression> condExpr, stepExpr;
};

class ForInStmt : public ForStmt {
DEFINE_ACCEPT_AND_INIT(ForInStmt)
private:
    ForInStmt(shared_ptr<Token> forTok,
              shared_ptr<Identifier> valueVar,
              shared_ptr<Identifier> indexVar,
              shared_ptr<Expression> rangeExpr,
              shared_ptr<Token> doTok,
              shared_ptr<Block> block,
              shared_ptr<Expression> condExpr,
              shared_ptr<Expression> stepExpr);
public:
    shared_ptr<Expression> rangeExpr;
    shared_ptr<Identifier> valueVar, indexVar;
};

class ForFromStmt : public ForStmt {
DEFINE_ACCEPT_AND_INIT(ForFromStmt)
protected:
    ForFromStmt(shared_ptr<Token> forTok,
                shared_ptr<Expression> itExpr,
                shared_ptr<Expression> floorExpr,
                shared_ptr<Expression> upperExpr,
                shared_ptr<Token> doTok,
                shared_ptr<Block> block,
                shared_ptr<Expression> condExpr,
                shared_ptr<Expression> stepExpr);
public:
    shared_ptr<Expression> itExpr, floorExpr, upperExpr;
};


class LoopStmt : public AstNode {
DEFINE_ACCEPT_AND_INIT(LoopStmt)
protected:
    LoopStmt(shared_ptr<Token> loopTok, shared_ptr<Block> block);
public:
    shared_ptr<Block> block;
};

class DoWhileStmt : public AstNode {
DEFINE_ACCEPT_AND_INIT(DoWhileStmt)
protected:
    DoWhileStmt(shared_ptr<Token> doTok,
                shared_ptr<Block> block,
                shared_ptr<Expression> cond);
public:
    bool singleLine;
    shared_ptr<Expression> condExpr;
    shared_ptr<Block> block;
};

class WhileStmt : public AstNode {
DEFINE_ACCEPT_AND_INIT(WhileStmt)
protected:
    WhileStmt(shared_ptr<Token> whileTok,
              shared_ptr<Expression> cond,
              shared_ptr<Block> block);
public:
    shared_ptr<Expression> condExpr;
    shared_ptr<Block> block;
};

class OnScopeExit : public AstNode {
DEFINE_ACCEPT_AND_INIT(OnScopeExit)
protected:
    OnScopeExit(shared_ptr<Token> oseTok, shared_ptr<Block> block);
public:
    shared_ptr<Block> block;
};

class Literal : public Expression {
DEFINE_ACCEPT_AND_INIT(Literal)
protected:
    Literal(shared_ptr<Token> tok);
public:
    shared_ptr<Token> token;
};

class CompoundLiteral : public Expression {
DEFINE_ACCEPT_AND_INIT(CompoundLiteral)
protected:
    CompoundLiteral();
    CompoundLiteral(shared_ptr<Token> left);
public:
    unsigned long minIndent = 0;
    bool isArray, isStruct, isImplicit;

    typedef std::pair<shared_ptr<AstNode>, shared_ptr<Expression>> kvPair;
    std::vector<kvPair> initializerList;

    void addKvPair(shared_ptr<kvPair> pair);
};

class FunctionDefinition : public Expression {
DEFINE_ACCEPT_AND_INIT(FunctionDefinition)
protected:
    FunctionDefinition(shared_ptr<Token> inlineTok,
                       shared_ptr<Token> noReturnTok,
                       shared_ptr<TypeSpecifier> returnType,
                       shared_ptr<Identifier> functionName,
                       std::vector<shared_ptr<FunctionParamDecl>> &paramDeclList,
                       bool hasVariableArg,
                       shared_ptr<Block> functionBody);
public:
    bool isAnonymous, isInline, isNoReturn, hasVariableArg;
    shared_ptr<TypeSpecifier> returnType;
    std::vector<shared_ptr<FunctionParamDecl>> paramDeclList;
    shared_ptr<Identifier> functionName;
    shared_ptr<Block> functionBody;
};

class UnaryArithmeticExpression : public Expression {
DEFINE_ACCEPT_AND_INIT(UnaryArithmeticExpression)
protected:
    UnaryArithmeticExpression(shared_ptr<Token> optr,
                              shared_ptr<Expression> rightExpr);

    UnaryArithmeticExpression(shared_ptr<Expression> leftExpr,
                              shared_ptr<Token> optr);
public:
    bool postfix;

    shared_ptr<Token> optr;
    shared_ptr<Expression> operand;
};

class BinaryExpression : public Expression {
DEFINE_ACCEPT_AND_INIT(BinaryExpression)
protected:
    BinaryExpression(shared_ptr<Expression> left,
                     shared_ptr<Token> optr,
                     shared_ptr<Expression> right);
public:
    shared_ptr<Expression> left, right;
    shared_ptr<Token> optr;
};

class BraceExpression : public Expression {
DEFINE_ACCEPT_AND_INIT(BraceExpression)
protected:
    BraceExpression(shared_ptr<Token> lParen,
                    shared_ptr<Expression> expr,
                    shared_ptr<Token> RParen);
public:
    shared_ptr<Expression> expr;
};

class IfExpression : public Expression {
DEFINE_ACCEPT_AND_INIT(IfExpression)
protected:
    IfExpression(shared_ptr<Token> ifTok,
                 shared_ptr<Token> unlikelyTok,
                 shared_ptr<Expression> condExpr,
                 shared_ptr<Block> thenBlock,
                 shared_ptr<Block> elseBlock = nullptr);

public:
    bool isUnlikely;

    shared_ptr<Expression> cond;
    shared_ptr<Block> thenBlock, elseBlock;
};

class LongjmpExpression : public Expression {
DEFINE_ACCEPT_AND_INIT(LongjmpExpression)
private:
    LongjmpExpression(shared_ptr<Token> longjmpTok,
                      shared_ptr<Expression> jmpBuf,
                      shared_ptr<Expression> intVal,
                      shared_ptr<Token> rParen);

public:
    shared_ptr<Expression> jmpBuf, intVal;
};

class VaArgExpression : public Expression {
DEFINE_ACCEPT_AND_INIT(VaArgExpression)
protected:
    VaArgExpression(shared_ptr<Token> VaArgTok,
                    shared_ptr<Expression> argumentExpr,
                    shared_ptr<TypeSpecifier> typeSpecifier,
                    shared_ptr<Token> rParen = nullptr);

public:
    shared_ptr<Expression> argumentExpr;
    shared_ptr<TypeSpecifier> typeSpecifier;
};

class OffsetOfExpression : public Expression {
DEFINE_ACCEPT_AND_INIT(OffsetOfExpression)
private:
    OffsetOfExpression(shared_ptr<Token> offsetOfTok,
                       shared_ptr<TypeSpecifier> typeSpecifier,
                       shared_ptr<Token> fieldName,
                       shared_ptr<Token> rParen);
public:
    shared_ptr<TypeSpecifier> typeSpecifier;
    shared_ptr<Token> fieldName;
};

class SizeOfExpression : public Expression {
DEFINE_ACCEPT_AND_INIT(SizeOfExpression)
private:
    SizeOfExpression(shared_ptr<Token> sizeofTok, shared_ptr<Expression> right);
    SizeOfExpression(shared_ptr<Token> sizeofTok,
                     shared_ptr<TypeSpecifier> right);
public:
    bool isExpr, isTypeSpecifier;
    shared_ptr<AstNode> operand;
};

class AlignOfExpression : public Expression {
DEFINE_ACCEPT_AND_INIT(AlignOfExpression)
private:
    AlignOfExpression(shared_ptr<Token> alignofTok,
                      shared_ptr<TypeSpecifier> typeSpecifier);
public:
    shared_ptr<TypeSpecifier> operand;
};

class ArrayAccess : public Expression {
DEFINE_ACCEPT_AND_INIT(ArrayAccess)
protected:
    ArrayAccess(shared_ptr<Expression> array,
                shared_ptr<Expression> index,
                shared_ptr<Token> rBracket);
public:
    bool hasExistTest;
    shared_ptr<Expression> arrayExpr, subscript;
};

class StructOrUnionFieldAccess : public Expression {
DEFINE_ACCEPT_AND_INIT(StructOrUnionFieldAccess)
protected:
    StructOrUnionFieldAccess(shared_ptr<Expression> obj,
                             shared_ptr<Token> query,
                             shared_ptr<Token> periodOrArrow,
                             shared_ptr<Identifier> field);
    StructOrUnionFieldAccess(shared_ptr<Expression> obj,
                             shared_ptr<Token> query,
                             shared_ptr<Token> periodOrArrow,
                             shared_ptr<Token> id);
public:
    bool isArrowAccess, hasExistTest;

    shared_ptr<Expression> obj;
    shared_ptr<Identifier> field;
};

class CastExpression : public Expression {
DEFINE_ACCEPT_AND_INIT(CastExpression)
protected:
    CastExpression(shared_ptr<Token> lParen,
                   shared_ptr<TypeSpecifier> specifier,
                   shared_ptr<Expression> expr);
public:
    shared_ptr<TypeSpecifier> typeSpecifier;
    shared_ptr<Expression> operand;
};

class ArgumentExpressionList : public Expression {
DEFINE_ACCEPT_AND_INIT(ArgumentExpressionList)
protected:
    ArgumentExpressionList() { }
public:
    std::vector<shared_ptr<Expression>> arguments;

    void addArguments(shared_ptr<Expression> argument) {
        this->arguments.push_back(argument);
        this->setNewLineAndSpaced(argument);
        argument->parent = this->get_shared_ptr();
    }
};

class UndeterminedCall : public Expression {
DEFINE_ACCEPT_AND_INIT(UndeterminedCall)
protected:
    UndeterminedCall(shared_ptr<Expression> caller,
                     shared_ptr<ArgumentExpressionList> arguments,
                     shared_ptr<Token> rParen = nullptr);
public:
    bool isImplicit;
    shared_ptr<Expression> callExpr;
    shared_ptr<ArgumentExpressionList> arguments;
};

class Identifier : public AstNode {
DEFINE_ACCEPT_AND_INIT(Identifier)
protected:
    Identifier(shared_ptr<Token> tok);
public:
    shared_ptr<Token> token;
};

//
// Declaration
//

class TypeSpecifier : public AstNode {
public:
    bool isConst = false,
         isVolatile = false,
         isRestrict = false,
         isImplicit = false,
         isAtomic = false,
         isBasicTypeSpecifier = false;

    shared_ptr<TypeSpecifier> subType;

    TypeSpecifier(shared_ptr<TypeSpecifier> sub) : subType(sub) { }

    TypeSpecifier() : subType(nullptr) { }
};

class BasicTypeSpecifier : public TypeSpecifier {
DEFINE_ACCEPT_AND_INIT(BasicTypeSpecifier)
protected:
    BasicTypeSpecifier(shared_ptr<Token> tok);
    BasicTypeSpecifier(shared_ptr<Token> tok, shared_ptr<Token> compOrImag);
public:
    shared_ptr<Token> token;
    bool isComplex, isImaginary;
};

class TypedefSpecifier : public TypeSpecifier {
DEFINE_ACCEPT_AND_INIT(TypedefSpecifier)
protected:
    TypedefSpecifier(shared_ptr<Token> tok);
public:
    shared_ptr<Token> token;
};

class PointerTypeSpecifier : public TypeSpecifier {
DEFINE_ACCEPT_AND_INIT(PointerTypeSpecifier)
protected:
    PointerTypeSpecifier(shared_ptr<Token> astrisk,
                         shared_ptr<TypeSpecifier> sub,
                         shared_ptr<Token> rParen = nullptr);
};

class Enumerator : public AstNode {
DEFINE_ACCEPT_AND_INIT(Enumerator)
protected:
    Enumerator(shared_ptr<Identifier> name,
               shared_ptr<Expression> value = nullptr);
public:
    shared_ptr<Identifier> name;
    shared_ptr<Expression> value;
};

class EnumSpecifier : public TypeSpecifier {
DEFINE_ACCEPT_AND_INIT(EnumSpecifier)
protected:
    EnumSpecifier(shared_ptr<Token> enumTok, shared_ptr<Identifier> name);
    EnumSpecifier(shared_ptr<Token> enumTok, shared_ptr<Token> name);
public:
    unsigned long minIndent;

    bool isAnonymous, noEnumerator = true;

    shared_ptr<Token> enumToken;
    shared_ptr<Identifier> specifierName;
    std::vector<shared_ptr<Enumerator>> enumerators;

    void addEnumerator(shared_ptr<Enumerator> e);
};

class StructOrUnionField : public AstNode {
DEFINE_ACCEPT_AND_INIT(StructOrUnionField)
protected:
    StructOrUnionField(shared_ptr<Identifier> id,
                       shared_ptr<Expression> len = nullptr);
    StructOrUnionField(shared_ptr<Token> id,
                       shared_ptr<Expression> len = nullptr);
public:
    bool isBitField;
    shared_ptr<Expression> bitLength;
    shared_ptr<Identifier> fieldName;
};

class StructOrUnionFieldDecl : public AstNode {
DEFINE_ACCEPT_AND_INIT(StructOrUnionFieldDecl)
protected:
    StructOrUnionFieldDecl(bool isTypedef, shared_ptr<TypeSpecifier> specifier);
public:
    bool isTypedef;
    shared_ptr<TypeSpecifier> fieldType;
    std::vector<shared_ptr<StructOrUnionField>> fieldList;

    void addField(shared_ptr<StructOrUnionField> field);
};

class StructOrUnionSpecifier : public TypeSpecifier {
DEFINE_ACCEPT_AND_INIT(StructOrUnionSpecifier)
protected:
    StructOrUnionSpecifier(shared_ptr<Token> structTok,
                           shared_ptr<Identifier> id);
    StructOrUnionSpecifier(shared_ptr<Token> structTok, shared_ptr<Token> id);
public:
    bool isAnonymous,
         isStruct,
         isUnion;

    unsigned long minIndent;

    std::vector<shared_ptr<StructOrUnionFieldDecl>> declList;
    shared_ptr<Identifier> specifierName;

    void addFieldDecl(shared_ptr<StructOrUnionFieldDecl> decl);
};

class RightTypeOperator : public TypeSpecifier {
public:
    void setSubType(shared_ptr<TypeSpecifier> sub) {
        this->subType = sub;
    }

    RightTypeOperator () { }
};

class ArrayTypeSpecifier : public RightTypeOperator {
DEFINE_ACCEPT_AND_INIT(ArrayTypeSpecifier)
protected:
    ArrayTypeSpecifier(shared_ptr<Token> lBracket, shared_ptr<Expression> size);
public:
    shared_ptr<Expression> arraySize;
};

class FunctionParamDecl: public AstNode {
DEFINE_ACCEPT_AND_INIT(FunctionParamDecl)
protected:
    FunctionParamDecl(shared_ptr<TypeSpecifier> pType,
                      shared_ptr<Identifier> paramName,
                      shared_ptr<Expression> defaultVal = nullptr);

    FunctionParamDecl(shared_ptr<TypeSpecifier> pType,
                      shared_ptr<Token> paramName,
                      shared_ptr<Expression> defaultVal = nullptr);
public:
    shared_ptr<TypeSpecifier> paramType;
    shared_ptr<Identifier> paramName;
    shared_ptr<Expression> defaultValue;
};

class FunctionTypeSpecifier : public RightTypeOperator {
DEFINE_ACCEPT_AND_INIT(FunctionTypeSpecifier)
protected:
    FunctionTypeSpecifier(shared_ptr<TypeSpecifier> retType);
public:
    bool hasVariableArg = false;
    std::vector<shared_ptr<FunctionParamDecl>> paramDeclList;

    void addParam(shared_ptr<FunctionParamDecl> paramDecl);
};

class DeclInit: public AstNode {
DEFINE_ACCEPT_AND_INIT(DeclInit)
protected:
    DeclInit(shared_ptr<Identifier> id, shared_ptr<Expression> init = nullptr);
    DeclInit(shared_ptr<Token> id, shared_ptr<Expression> init = nullptr);
public:
    shared_ptr<Identifier> identifier;
    shared_ptr<Expression> initValue;
};

class VarOrTypedefDecl: public AstNode {
DEFINE_ACCEPT_AND_INIT(VarOrTypedefDecl)
protected:
    VarOrTypedefDecl(shared_ptr<Token> storageSpecifier,
                     shared_ptr<TypeSpecifier> typeSpecifier);
public:
    shared_ptr<TypeSpecifier> typeSpecifier;
    std::vector<shared_ptr<DeclInit>> declInitList;

    void addDeclInit(shared_ptr<DeclInit> declInit);

    bool isStatic,
         isRegister,
         isThreadLocal,
         isTypedef;
};

class StaticAssertDecl : public AstNode {
DEFINE_ACCEPT_AND_INIT(StaticAssertDecl)
protected:
    StaticAssertDecl(shared_ptr<Token> saTok,
                     shared_ptr<Expression> testExpr,
                     shared_ptr<Expression> errExpr,
                     shared_ptr<Token> rParen);
public:
    shared_ptr<Expression> testExpr, errExpr;
};

class PragmaDecl : public AstNode {
DEFINE_ACCEPT_AND_INIT(PragmaDecl)
protected:
    PragmaDecl(shared_ptr<Token> pragmaTok);
public:
    std::vector<shared_ptr<Token>> tokens;
    void addToken(shared_ptr<Token> tok);
};

class Annotation : public AstNode {
DEFINE_ACCEPT_AND_INIT(Annotation)
protected:
    Annotation(shared_ptr<Token> atSign, shared_ptr<Identifier> annoName);
    Annotation(shared_ptr<Token> atSign,
               shared_ptr<Identifier> annoName,
               shared_ptr<ArgumentExpressionList> params,
               shared_ptr<Token> rParen = nullptr);
    Annotation(shared_ptr<Token> atSign, shared_ptr<Token> annoName);
    Annotation(shared_ptr<Token> atSign,
               shared_ptr<Token> annoName,
               shared_ptr<ArgumentExpressionList> params,
               shared_ptr<Token> rParen = nullptr);
public:
    shared_ptr<Identifier> annoName;
    shared_ptr<ArgumentExpressionList> params;
};

class PreProcessCondition : public AstNode {
DEFINE_ACCEPT_AND_INIT(PreProcessCondition)
protected:
    PreProcessCondition(shared_ptr<Token> directive,
                        shared_ptr<Expression> cond,
                        shared_ptr<AstNode> thenBlock);
public:
    shared_ptr<Token> directive;
    shared_ptr<Expression> cond;
    shared_ptr<AstNode> stmtBlock;
    shared_ptr<PreProcessCondition> elseGroup;
};

#undef DEFINE_ACCEPT_AND_INIT

#define V(nodeClass) \
    inline bool AstNode::is##nodeClass() {       \
        return typeid(*this) == typeid(nodeClass);\
    }   
    FOREACH_AST_NODE(V)
#undef V
}  // namespace Parsing
}  // namespace ppc

#endif
