#include <fstream>
#include <memory>
#include <string>
#include <sstream>
#include <iostream>

#include <Ast.h>
#include <DataList.h>
#include <AstGraphGenerator.h>

#define bool2str(val) ((val) ? "true" : "false")
namespace ppc {
namespace Parsing {

using std::make_shared;

template <typename nodeType>
inline std::shared_ptr<AstGraphGenerator::AstGraphNode>
AstGraphGenerator::addNode(nodeType const &node) {
    auto p = make_shared<AstGraphGenerator::AstGraphNode>(node);
    this->nodes.push_back(p);
    return p;
}

# define V(T)\
template <>                                                         \
inline std::shared_ptr<AstGraphGenerator::AstGraphNode>             \
AstGraphGenerator::addNode(T const &node) {                         \
    auto p = make_shared<AstGraphGenerator::AstGraphNode>(node);    \
    this->nodes.push_back(p);                                       \
    if (node.isConst) p->nodeFieldsSuffix += " | const: true";      \
    if (node.isVolatile) p->nodeFieldsSuffix += " | volatile: true";\
    if (node.isRestrict) p->nodeFieldsSuffix += " | restrict: true";\
    if (node.isAtomic) p->nodeFieldsSuffix += " | atomic: true";    \
    return p;                                                       \
}
FOREACH_TYPE_SPECIFIER_NODE(V)
#undef V

std::string AstGraphGenerator::getGraphSource() {
    std::stringstream result;

    result <<"digraph g {\n"
             "graph [\n"
             "    rankdir = \"LR\"\n"
             "];\n"
             "node [\n"
             "    fontsize = \"16\"\n"
             "    shape = \"ellipse\"\n"
             "];\n"
             "edge [];\n";

    for (auto node: this->nodes) {
        result << "\"node" << node->id << "\" [" << std::endl
        << "    label = \"" << node->nodeFields << node->nodeFieldsSuffix
        <<'\"' << std::endl
        << "    shape = \"record\"" << std::endl
        << "];" << std::endl;
    }

    for (auto edge: this->edges) {
        result << "\"node" << edge->fromNode << "\":f" << edge->fromField <<
        " -> "
        << "\"node" << edge->toNode << "\":f" << edge->toField << ';'
        << std::endl;
    }

    result << '}';

    return static_cast<std::string>(result.str());
}

void inline AstGraphGenerator::AstGraphNode::addField(std::string add) {
    std::ostringstream s;
    for (auto c : add) {
        switch (c) {
            case '\\': s << "\\\\"; break;
            case '"': s << "\\\""; break;
            case '\'': s << "\\\'"; break;
            case '<': s << "\\<"; break;
            case '>': s << "\\>"; break;
            case '/': s << "\\/"; break;
            case '\b': s << "\\b"; break;
            case '\f': s << "\\f"; break;
            case '\n': s << "\\n"; break;
            case '\r': s << "\\r"; break;
            case '\t': s << "\\t"; break;
            default: s << c; break;
        }
    }
    this->nodeFields += " |<f" + std::to_string(++this->nFields) + ">" + s.str();
}

void inline AstGraphGenerator::addEdge(AstNode const &fromNode,
                                       graphCount fromField,
                                       AstNode const &toNode,
                                       graphCount toField)
{
    this->edges.push_back(make_shared<AstGraphEdge>(fromNode.id,
                                                    fromField,
                                                    toNode.id,
                                                    toField));
}

void AstGraphGenerator::visit(TranslationUnit &node)
{
    auto tUnit = this->addNode(node);

    for (auto child : node.statements) {
        this->addEdge(node, tUnit->nFields, *child);
    }
}

void AstGraphGenerator::visit(Block &node) {
    auto block = this->addNode(node);
    block->addField("Statements");

    for (auto stmt : node.statements) {
        this->addEdge(node, 1, *stmt);
    }
}

void AstGraphGenerator::visit(OnScopeExit &node) {
    auto oseNode = this->addNode(node);
    oseNode->addField("Block");
    this->addEdge(node, 1, *node.block);
}

void AstGraphGenerator::visit(WhileStmt &node) {
    auto whileStmt = this->addNode(node);

    whileStmt->addField("Condition");
    this->addEdge(node, 1, *node.condExpr);
    whileStmt->addField("Loop Body");
    this->addEdge(node, 2, *node.block);
}

void AstGraphGenerator::visit(DoWhileStmt &node) {
    auto doWhileStmt = this->addNode(node);

    doWhileStmt->addField("Loop Body");
    this->addEdge(node, 1, *node.block);
    doWhileStmt->addField("Condition");
    this->addEdge(node, 2, *node.condExpr);
}

void AstGraphGenerator::visit(LoopStmt &node) {
    auto loopStmt = this->addNode(node);
    loopStmt->addField("Loop Body");
    this->addEdge(node, 1, *node.block);
}

void AstGraphGenerator::visit(ForFromStmt &node) {
    auto forFromStmt = this->addNode(node);

    forFromStmt->addField("Iterator");
    this->addEdge(node, 1, *node.itExpr);

    forFromStmt->addField("Floor Expression");
    this->addEdge(node, 2, *node.floorExpr);

    forFromStmt->addField("Upper Expression");
    this->addEdge(node, 3, *node.upperExpr);

    forFromStmt->addField("Loop Body");
    this->addEdge(node, 4, *node.block);

    if (node.condExpr) {
        forFromStmt->addField("Prerequisite");
        this->addEdge(node, forFromStmt->nFields, *node.condExpr);
    }

    if (node.stepExpr) {
        forFromStmt->addField("Step Expression");
        this->addEdge(node, forFromStmt->nFields, *node.stepExpr);
    }
}

void AstGraphGenerator::visit(ForInStmt &node) {
    auto forInStmt = this->addNode(node);

    forInStmt->addField("Value Variable");
    this->addEdge(node, 1, *node.valueVar);

    if (node.indexVar) {
        forInStmt->addField("Index Variable");
        this->addEdge(node, forInStmt->nFields, *node.indexVar);
    }

    forInStmt->addField("Range Expression");
    this->addEdge(node, forInStmt->nFields, *node.rangeExpr);

    forInStmt->addField("Loop Body");
    this->addEdge(node, forInStmt->nFields, *node.block);

    if (node.condExpr) {
        forInStmt->addField("Prerequisite");
        this->addEdge(node, forInStmt->nFields, *node.condExpr);
    }

    if (node.stepExpr) {
        forInStmt->addField("Step Expression");
        this->addEdge(node, forInStmt->nFields, *node.stepExpr);
    }
}

void AstGraphGenerator::visit(SwitchLabel &node) {
    auto switchLabel = this->addNode(node);

    if (!node.isDefault) {
        switchLabel->addField("Condition");
    }

    for (auto expr : node.condList) {
        this->addEdge(node, 1, *expr);
    }

    switchLabel->addField("Action Block");
    this->addEdge(node, switchLabel->nFields, *node.action);
}

void AstGraphGenerator::visit(SwitchStmt &node) {
    auto switchStmt = this->addNode(node);

    switchStmt->addField("Operand");
    this->addEdge(node, 1, *node.operand);

    for (auto label : node.labelList) {
        switchStmt->addField(label->isCase ? "Case"
                                           : label->isWhen ? "When"
                                                           : "Default");
        this->addEdge(node, switchStmt->nFields, *label);
    }

    if (node.onScopeExit) {
        switchStmt->addField("On Scope Exit");
        this->addEdge(node, switchStmt->nFields, *node.onScopeExit);
    }
}

void AstGraphGenerator::visit(GotoStmt &node) {
    auto gotoStmt = this->addNode(node);

    gotoStmt->addField(node.labelName->value);
}

void AstGraphGenerator::visit(GotoLabel &node) {
    auto gotoLabel = this->addNode(node);

    gotoLabel->addField(node.labelName->value);
}

void AstGraphGenerator::visit(ReturnStmt &node) {
    auto returnStmt = this->addNode(node);

    if (node.returnExpr) {
        returnStmt->addField("Return Expression");
        this->addEdge(node, 1, *node.returnExpr);
    }
}

void AstGraphGenerator::visit(ContinueOrBreak &node) {
    auto cOb = this->addNode(node);
    cOb->nodeFields = "<f0>" + node.token->value;
}

void AstGraphGenerator::visit(StaticAssertDecl &node) {
    auto sa = this->addNode(node);

    sa->addField("Test Expression");
    this->addEdge(node, 1, *node.testExpr);

    sa->addField("Error Message");
    this->addEdge(node, 2, *node.errExpr);
}

void AstGraphGenerator::visit(PragmaDecl &node) {
    auto pragmaDecl = this->addNode(node);

    auto field = std::string();
    for (auto tok : node.tokens) {
        field += tok->value + " ";
    }
    pragmaDecl->addField(field);
}

void AstGraphGenerator::visit(Annotation &node) {
    auto anno = this->addNode(node);

    anno->addField("Name");
    this->addEdge(node, 1, *node.annoName);

    if (node.params) {
        anno->addField("Params");
        this->addEdge(node, 2, *node.params);
    }
}

void AstGraphGenerator::visit(Literal &node) {
    auto literal = this->addNode(node);

    switch (node.token->type) {
#define V(tType) case tokType::tType:\
        literal->addField(#tType": " + node.token->value);break;
        FOREACH_BASIC_TYPE_LITERAL_TOKTYPE(V)
#undef V
        default:
            break;
    }
}

void AstGraphGenerator::visit(CompoundLiteral &node) {
    auto compoundLiteral = this->addNode(node);

    if (node.isArray) {
        compoundLiteral->nodeFields = "<f0>Array Literal";
    } else {
        compoundLiteral->nodeFields = "<f0>Struct Literal";
    }

    for (auto kvPair : node.initializerList) {
        if (kvPair.first) {
            if (auto idKey = std::dynamic_pointer_cast<Identifier>(kvPair.first)) {
                compoundLiteral->addField("Key: " + idKey->token->value);
                this->addEdge(node, compoundLiteral->nFields, *kvPair.first);

                compoundLiteral->addField("Value of " + idKey->token->value);
                this->addEdge(node, compoundLiteral->nFields, *kvPair.second);
            }else {
                auto expr = std::static_pointer_cast<Expression>(kvPair.first);

                compoundLiteral->addField("Expression Key");
                this->addEdge(node, compoundLiteral->nFields, *kvPair.first);
                compoundLiteral->addField("Value");
                this->addEdge(node, compoundLiteral->nFields, *kvPair.second);
            }
        } else {
            if (node.isArray) {
                compoundLiteral->addField("Element Value");
            } else {
                compoundLiteral->addField("Field Value");
            }
            this->addEdge(node, compoundLiteral->nFields, *kvPair.second);
        }
    }
}

void AstGraphGenerator::visit(FunctionDefinition &node) {
    auto funcDef = this->addNode(node);

    if (node.isInline) {
        funcDef->addField("inline: true");
    }

    if (node.isNoReturn) {
        funcDef->addField("noreturn: true");
    }

    funcDef->addField("Return Type");
    this->addEdge(node, funcDef->nFields, *node.returnType);

    if (node.functionName) {
        funcDef->addField("Fcuntion name");
        this->addEdge(node, funcDef->nFields, *node.functionName);
    }

    if (node.paramDeclList.empty()) {
        funcDef->addField("Params: (void)");
    } else {
        for (auto param : node.paramDeclList) {
            funcDef->addField("Params: "
                                         + param->paramName->token->value);

            this->addEdge(node, funcDef->nFields, *param);
        }

        funcDef->addField(std::string("Variable Argument: ") +
                          bool2str(node.hasVariableArg)
        );
    }

    funcDef->addField("Function Body");
    this->addEdge(node, funcDef->nFields, *node.functionBody);
}

void AstGraphGenerator::visit(UnaryArithmeticExpression &node) {
    auto uExpr = this->addNode(node);

    uExpr->addField("Operator: '" + node.optr->value + '\'');
    uExpr->addField("Operand");
    this->addEdge(node, 2, *node.operand);
}

void AstGraphGenerator::visit(BinaryExpression &node) {
    auto bExpr = this->addNode(node);

    bExpr->addField("Left Operand");
    bExpr->addField("Operator: '" + node.optr->value + '\'');
    bExpr->addField("Right Operand");

    this->addEdge(node, 1, *node.left);
    this->addEdge(node, 3, *node.right);
}

void AstGraphGenerator::visit(BraceExpression &node) {
    auto expr = this->addNode(node);

    expr->addField("Braced Expression");
    this->addEdge(node, 1, *node.expr);
}

void AstGraphGenerator::visit(IfExpression &node) {
    auto ifExpr = this->addNode(node);

    ifExpr->addField("Condition");
    this->addEdge(node, ifExpr->nFields, *node.cond);

    ifExpr->addField("True Block");
    this->addEdge(node, ifExpr->nFields, *node.thenBlock);

    if (node.elseBlock) {
        ifExpr->addField("False Block");
        this->addEdge(node, ifExpr->nFields, *node.elseBlock);
    }
}

void AstGraphGenerator::visit(OffsetOfExpression &node) {
    auto offsetOfExpr = this->addNode(node);

    offsetOfExpr->addField("Type Specifier");
    this->addEdge(node, 1, *node.typeSpecifier);

    offsetOfExpr->addField("Field Name: " + node.fieldName->value);
}

void AstGraphGenerator::visit(SizeOfExpression &node) {
    auto sizeofExpr = this->addNode(node);

    sizeofExpr->addField("Operand");
    this->addEdge(node, 1, *node.operand);
}

void AstGraphGenerator::visit(LongjmpExpression &node) {
    auto longjmpExpr = this->addNode(node);

    longjmpExpr->addField("Jump Buffer");
    this->addEdge(node, 1, *node.jmpBuf);
    longjmpExpr->addField("Return Value");
    this->addEdge(node, 2, *node.intVal);
}

void AstGraphGenerator::visit(VaArgExpression &node) {
    auto vaArgExpr = this->addNode(node);

    vaArgExpr->addField("Argument List Pointer");
    this->addEdge(node, 1, *node.argumentExpr);

    if (node.typeSpecifier) {
        vaArgExpr->addField("Argument Type");
        this->addEdge(node, 2, *node.typeSpecifier);
    }
}

void AstGraphGenerator::visit(AlignOfExpression &node) {
    auto alignOfExpr = this->addNode(node);

    alignOfExpr->addField("Operand");
    this->addEdge(node, 1, *node.operand);
}

void AstGraphGenerator::visit(ArrayAccess &node) {
    auto arrayExpr = this->addNode(node);

    arrayExpr->addField("Array Expression");
    this->addEdge(node, 1, *node.arrayExpr);
    arrayExpr->addField("SubScript");
    this->addEdge(node, 2, *node.subscript);
}

void AstGraphGenerator::visit(StructOrUnionFieldAccess &node) {
    auto fieldExpr = this->addNode(node);

    fieldExpr->addField(std::string("Test Existence: ")
                        + bool2str(node.hasExistTest));

    fieldExpr->addField(std::string("Access Operator: ")
                        + (node.isArrowAccess ? "->" : "."));

    fieldExpr->addField("Object");
    this->addEdge(node, 3, *node.obj);

    fieldExpr->addField("Field");
    this->addEdge(node, 4, *node.field);
}

void AstGraphGenerator::visit(CastExpression &node) {
    auto castExpr = this->addNode(node);

    castExpr->addField("Cast To");
    this->addEdge(node, 1, *node.typeSpecifier);

    castExpr->addField("Operand");
    this->addEdge(node, 2, *node.operand);
}

void AstGraphGenerator::visit(ArgumentExpressionList &node) {
    auto arguments = this->addNode(node);

    if (node.arguments.empty()) {
        arguments->addField("(void)");
    } else {
        arguments->addField("Arguments");
        for (auto argument : node.arguments) {
            this->addEdge(node, 1, *argument);
        }
    }
}

void AstGraphGenerator::visit(UndeterminedCall &node) {
    auto arguments = this->addNode(node);

    arguments->addField("Call Expression");
    this->addEdge(node, 1, *node.callExpr);

    arguments->addField("Arguments List");
    this->addEdge(node, 2, *node.arguments);
}

void AstGraphGenerator::visit(EnumSpecifier &node) {
    auto specifier = this->addNode(node);

    if (!node.isAnonymous) {
        specifier->addField("Specifier Name");
        this->addEdge(node, 1, *node.specifierName);
    }

    if (!node.enumerators.empty()) {
        specifier->addField("Enumerators");
        for (auto e: node.enumerators) {
            this->addEdge(node, 2, *e);
        }
    }

    if (node.isImplicit) {
        specifier->addField("Implicit: true");
    } else {
        specifier->addField("Implicit: false");
    }

    if (node.isAnonymous) {
        specifier->addField("Anonymous: true");
    } else {
        specifier->addField("Anonymous: false");
    }
}

void AstGraphGenerator::visit(Enumerator &node) {
    auto enumerator = this->addNode(node);
    
    enumerator->addField("Name");
    this->addEdge(node, 1, *node.name);
    
    if (node.value) {
        enumerator->addField("Value Expression");
        this->addEdge(node, 2, *node.value);
    }
}

void AstGraphGenerator::visit(DeclInit &node) {
    auto declInit = this->addNode(node);

    declInit->addField("Name");
    this->addEdge(node, 1, *node.identifier);

    if (node.initValue) {
        declInit->addField("Init value");
        this->addEdge(node, 2, *node.initValue);
    }
}

void AstGraphGenerator::visit(VarOrTypedefDecl &node) {
    auto varOrTypedefDecl = this->addNode(node);
    if (node.isTypedef) {
        varOrTypedefDecl->nodeFields = "<f0>Typedef Decl";
    } else {
        varOrTypedefDecl->nodeFields = "<f0>Var Decl";
    }

    varOrTypedefDecl->addField("Type Specifier");
    this->addEdge(node, 1, *node.typeSpecifier);

    varOrTypedefDecl->addField("Init List");
    for (auto decl : node.declInitList) {
        this->addEdge(node, 2, *decl);
    }

    if (node.isStatic) {
        varOrTypedefDecl->addField("static: true");
    }

    if (node.isThreadLocal) {
        varOrTypedefDecl->addField("thread_local: true");
    }

    if (node.isTypedef) {
        varOrTypedefDecl->addField("typedef: true");
    }

    if (node.isRegister) {
        varOrTypedefDecl->addField("register: true");
    }
}

void AstGraphGenerator::visit(StructOrUnionField &node) {
    auto field = this->addNode(node);

    auto fieldDecl = node.parent.lock();
    if (fieldDecl) {
        auto specifier =
        std::static_pointer_cast<StructOrUnionSpecifier>(fieldDecl->parent.lock());

        if (specifier) {
            field->nodeFields = specifier->isStruct?"<f0>Struct":"<f0>Union"
                + (node.isBitField?std::string(" Bit"):std::string(" ")
                       + std::string(" Fields"));

            field->addField("Field Name");
            this->addEdge(node, 1, *node.fieldName);

            if (node.isBitField) {
                field->addField("Bit Field Length");
                this->addEdge(node, 2, *node.bitLength);
            }
        }
    }
}

void AstGraphGenerator::visit(StructOrUnionFieldDecl &node) {
    auto decl = this->addNode(node);
    auto parent = node.parent.lock();

    decl->nodeFields = std::string("<f0>") +
            (std::static_pointer_cast<StructOrUnionSpecifier>(parent)
                       ->isStruct?"Struct Fields Decl":"Union Fields Decl");
    decl->addField("TypeSpecifier");

    this->addEdge(node, 1, *node.fieldType);

    if (!node.fieldList.empty()) {
        decl->addField("Fields");
        for (auto field: node.fieldList) {
            this->addEdge(node, 2, *field);
        }
    }

    if (node.isTypedef) {
        decl->addField("typedef: true");
    }
}

void AstGraphGenerator::visit(StructOrUnionSpecifier &node) {
    auto specifier = this->addNode(node);
    specifier->nodeFields = node.isStruct?"<f0>Struct":"<f0>Union"
                           + std::string("Specifier");
    
    if (!node.isAnonymous) {
        specifier->addField("Specifier Name");
        this->addEdge(node, 1, *node.specifierName);
    }
    
    if (!node.declList.empty()) {
        specifier->addField("Field Declarations");
        for (auto decl: node.declList) {
            this->addEdge(node, node.isAnonymous?1:2, *decl);
        }
    }

    if (node.isImplicit) {
        specifier->addField("Implicit: true");
    } else {
        specifier->addField("Implicit: false");
    }

    if (node.isAnonymous) {
        specifier->addField("Anonymous: true");
    } else {
        specifier->addField("Anonymous: false");
    }
}

void AstGraphGenerator::visit(ArrayTypeSpecifier &node) {
    auto specifier = this->addNode(node);

    specifier->addField("Size");
    this->addEdge(node, 1, *node.arraySize);

    specifier->addField("Subtype");
    this->addEdge(node, 2, *node.subType);
}

void AstGraphGenerator::visit(PointerTypeSpecifier &node) {
    auto specifier = this->addNode(node);

    specifier->addField("Point to");
    this->addEdge(node, 1, *node.subType);
}

void AstGraphGenerator::visit(FunctionTypeSpecifier &node) {
    auto func = this->addNode(node);

    func->addField("Return type");
    this->addEdge(node, 1, *node.subType);

    if (!node.paramDeclList.empty()) {
        func->addField("Param list");
        for (auto param: node.paramDeclList) {
            this->addEdge(node, 2, *param);
        }
    }
    func->addField(std::string("Variable Argument: ") +
                     bool2str(node.hasVariableArg));
}

void AstGraphGenerator::visit(FunctionParamDecl &node) {
    auto param = this->addNode(node);

    param->addField("Param Type");
    this->addEdge(node, 1, *node.paramType);

    if (node.paramName) {
        param->addField("Param Name");
        this->addEdge(node, param->nFields, *node.paramName);
    }

    if (node.defaultValue) {
        param->addField("Default Value");
        this->addEdge(node, param->nFields, *node.defaultValue);
    }
}

void AstGraphGenerator::visit(BasicTypeSpecifier &node) {
    auto basicType = this->addNode(node);
    basicType->addField(node.token->value);
    if (node.isImaginary || node.isComplex) {
        basicType->addField(
                std::string("complex: ") + bool2str(node.isComplex));
        basicType->addField(
                std::string("imaginary: ") + bool2str(node.isImaginary));
    }
}

void AstGraphGenerator::visit(PreProcessCondition &node) {
    auto directive = this->addNode(node);
    directive->addField("Directive: " + node.directive->value);

    if (node.cond) {
        directive->addField("Condition");
        this->addEdge(node, 2, *node.cond);
    }

    directive->addField("Block");
    this->addEdge(node, directive->nFields, *node.stmtBlock);

    if (node.elseGroup) {
        directive->addField("Else Group");
        this->addEdge(node, directive->nFields, *node.elseGroup);
    }
}

void AstGraphGenerator::visit(TypedefSpecifier &node) {
    auto specifier = this->addNode(node);

    specifier->addField(node.token->value);
}

void AstGraphGenerator::visit(Identifier &node)
{
    auto id = this->addNode(node);

    id->addField(node.token->value);
}

} // namespace Parsing
} // namespace ppc