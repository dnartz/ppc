#include <Ast.h>
#include <AstVisitor.h>

namespace ppc {
namespace Parsing {

unsigned long AstNode::nNodes = 0;

void TranslationUnit::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    for (auto child : this->statements) {
        child->accept(visitor);
    }
}

void Identifier::accept(AstVisitor &visitor) {
    visitor.visit(*this);
}

void Block::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    for (auto s : this->statements) {
        s->accept(visitor);
    }
}

void OnScopeExit::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->block->accept(visitor);
}

void WhileStmt::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->condExpr->accept(visitor);
    this->block->accept(visitor);
}

void DoWhileStmt::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->block->accept(visitor);
    this->condExpr->accept(visitor);
}

void LoopStmt::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->block->accept(visitor);
}

void ForFromStmt::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->itExpr->accept(visitor);
    this->floorExpr->accept(visitor);
    this->upperExpr->accept(visitor);
    if (this->condExpr) this->condExpr->accept(visitor);
    this->block->accept(visitor);
    if (this->stepExpr) this->stepExpr->accept(visitor);
}

void ForInStmt::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->valueVar->accept(visitor);
    if (this->indexVar) this->indexVar->accept(visitor);
    this->rangeExpr->accept(visitor);
    if (this->condExpr) this->condExpr->accept(visitor);
    this->block->accept(visitor);
    if (this->stepExpr) this->stepExpr->accept(visitor);
}

void GotoStmt::accept(AstVisitor &visitor) {
    visitor.visit(*this);
}

void GotoLabel::accept(AstVisitor &visitor) {
    visitor.visit(*this);
}

void ReturnStmt::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    if (this->returnExpr) {
        this->returnExpr->accept(visitor);
    }
}

void ContinueOrBreak::accept(AstVisitor &visitor) {
    visitor.visit(*this);
}

void SwitchStmt::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->operand->accept(visitor);

    for (auto label : this->labelList) {
        label->accept(visitor);
    }

    if (this->onScopeExit) {
        this->onScopeExit->accept(visitor);
    }
}

void SwitchLabel::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    for (auto cond : this->condList) {
        cond->accept(visitor);
    }

    this->action->accept(visitor);
}

void StaticAssertDecl::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->testExpr->accept(visitor);
    this->errExpr->accept(visitor);
}

void PragmaDecl::accept(AstVisitor &visitor) {
    visitor.visit(*this);
}

void Annotation::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->annoName->accept(visitor);
    if (this->params) {
        this->params->accept(visitor);
    }
}

void Literal::accept(AstVisitor &visitor) {
    visitor.visit(*this);
}

void CompoundLiteral::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    for (auto kvPair : this->initializerList) {
        if (kvPair.first) {
            kvPair.first->accept(visitor);
        }

        kvPair.second->accept(visitor);
    }
}

void FunctionDefinition::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->returnType->accept(visitor);
    if (this->functionName) {
        this->functionName->accept(visitor);
    }

    for (auto param : this->paramDeclList) {
        param->accept(visitor);
    }

    this->functionBody->accept(visitor);
}

void UnaryArithmeticExpression::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->operand->accept(visitor);
}

void BinaryExpression::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->left->accept(visitor);
    this->right->accept(visitor);
}

void BraceExpression::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->expr->accept(visitor);
}

void IfExpression::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->cond->accept(visitor);
    this->thenBlock->accept(visitor);

    if (this->elseBlock) {
        this->elseBlock->accept(visitor);
    }
}

void LongjmpExpression::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->jmpBuf->accept(visitor);
    this->intVal->accept(visitor);
}

void VaArgExpression::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->argumentExpr->accept(visitor);
    if (this->typeSpecifier) {
        this->typeSpecifier->accept(visitor);
    }
}

void OffsetOfExpression::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->typeSpecifier->accept(visitor);
}

void SizeOfExpression::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->operand->accept(visitor);
}

void AlignOfExpression::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->operand->accept(visitor);
}

void ArrayAccess::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->arrayExpr->accept(visitor);
    this->subscript->accept(visitor);
}

void StructOrUnionFieldAccess::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->obj->accept(visitor);
    this->field->accept(visitor);
}

void CastExpression::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->typeSpecifier->accept(visitor);
    this->operand->accept(visitor);
}

void ArgumentExpressionList::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    for (auto argument : this->arguments) {
        argument->accept(visitor);
    }
}

void UndeterminedCall::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->callExpr->accept(visitor);
    this->arguments->accept(visitor);
}

void Enumerator::accept(AstVisitor &visitor) {
    visitor.visit(*this);
    this->name->accept(visitor);
    if (this->value) {
        this->value->accept(visitor);
    }
}

void EnumSpecifier::accept(AstVisitor &visitor) {
    visitor.visit(*this);
    if (!this->isAnonymous) {
        this->specifierName->accept(visitor);
    }

    for (auto e:this->enumerators) {
        e->accept(visitor);
    }
}

void StructOrUnionSpecifier::accept(AstVisitor &visitor) {
    visitor.visit(*this);
    if (!this->isAnonymous) {
        this->specifierName->accept(visitor);
    }

    for (auto fieldDecl: this->declList) {
        fieldDecl->accept(visitor);
    }
}

void StructOrUnionFieldDecl::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->fieldType->accept(visitor);

    for (auto field: this->fieldList) {
        field->accept(visitor);
    }
}

void StructOrUnionField::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->fieldName->accept(visitor);

    if (this->bitLength) {
        this->bitLength->accept(visitor);
    }
}

void DeclInit::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->identifier->accept(visitor);

    if (this->initValue) {
        this->initValue->accept(visitor);
    }
}

void VarOrTypedefDecl::accept(AstVisitor &visitor) {
    visitor.visit(*this);

    this->typeSpecifier->accept(visitor);
    for (auto init : this->declInitList) {
        init->accept(visitor);
    }
}

void BasicTypeSpecifier::accept(AstVisitor &visitor) {
    visitor.visit(*this);
}

void PointerTypeSpecifier::accept(AstVisitor &visitor) {
    visitor.visit(*this);
    this->subType->accept(visitor);
}

void ArrayTypeSpecifier::accept(AstVisitor &visitor) {
    visitor.visit(*this);
    this->arraySize->accept(visitor);
    this->subType->accept(visitor);
}

void FunctionParamDecl::accept(AstVisitor &visitor) {
    visitor.visit(*this);
    this->paramType->accept(visitor);

    if (this->paramName) {
        this->paramName->accept(visitor);
    }

    if (this->defaultValue) {
        this->defaultValue->accept(visitor);
    }
}

void FunctionTypeSpecifier::accept(AstVisitor &visitor) {
    visitor.visit(*this);
    this->subType->accept(visitor);
    for (auto param : this->paramDeclList) {
        param->accept(visitor);
    }
}

void TypedefSpecifier::accept(AstVisitor &visitor) {
    visitor.visit(*this);
}

void PreProcessCondition::accept(AstVisitor &visitor) {
    visitor.visit(*this);
    if (this->cond) {
        this->cond->accept(visitor);
    }

    this->stmtBlock->accept(visitor);

    if (this->elseGroup) {
        this->elseGroup->accept(visitor);
    }
}

} // namespace Parsing
} // namespace ppc
