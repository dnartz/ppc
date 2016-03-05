#include <memory>

#include <Parser.h>
#include <Ast.h>
#include <DataList.h>

namespace ppc {
namespace Parsing {

using ppc::tokType;
using ppc::Diagnostic::DiagnosticLevel;

using std::shared_ptr;
using std::make_shared;

// StorageSpecifier:
//    thread_local
//    static
//    typedef
//    register
shared_ptr<Token> Parser::parseStorageSpecifier(tokIt &begin) {
    shared_ptr<Token> specifier;

    if ((specifier = CONSUME(TYPEDEF))
        || (specifier = CONSUME(STATIC))
        || (specifier = CONSUME(THREAD_LOCAL))
        || (specifier = CONSUME(REGISTER_SPECIFIER)))
    {
        return specifier;
    } else {
        return nullptr;
    }
}

// VarDecl:
//     StorageSpecifierSequence? TypeSpecifier VarInitList
shared_ptr <VarOrTypedefDecl> Parser::parseVarDecl(tokIt &begin)
{
    MARK();
    auto storageSpecifier = this->parseStorageSpecifier(begin);

    shared_ptr<TypeSpecifier> typeSpecifier;
    if (!(typeSpecifier = this->parseTypeSpecifier(begin))) {
        PARSE_ERROR("Expect type specifier.");
        RELEASE_AND_RETURN();
    }

    auto decl = VarOrTypedefDecl::init(storageSpecifier, typeSpecifier);

    if (!this->parseVarInitList(begin, decl)) {
        PARSE_ERROR("Expect declaration init list.");
        RELEASE_AND_RETURN();
    }

    return decl;
}

// VarInit:
//     Identifier | Identifier Equals Expression
//
// VarInitList:
//     VarInit (Comma VarInit)*
bool Parser::parseVarInitList(tokIt &begin, shared_ptr <VarOrTypedefDecl> decl)
{
    while (true) {
        shared_ptr<Token> id;
        if (!(id = CONSUME(IDENTIFIER))) {
            break;
        } else {
            shared_ptr<Expression> initValue;
            if (CONSUME(EQUALS)) initValue = this->parseExpression(begin);
            decl->addDeclInit(DeclInit::init(id, initValue));
        }

        if (!CONSUME(COMMA)) {
            break;
        }
    }

    return !decl->declInitList.empty();
}


// TypeQualifier:
//     const | restrict | volatile | atomic
shared_ptr<Token> inline Parser::parseTypeQualifier(tokIt &begin) {
    shared_ptr<Token> qualifier;

#define V(type) || (qualifier = CONSUME(type))
    if (false FOREACH_TYPE_QUALIFIER_TOKTYPE(V))
#undef V
    {
        return qualifier;
    } else {
        return nullptr;
    }
}

#define DUP_QUALIFIER_ERROR(qualifierStr)                           \
    do {                                                            \
        this->error(DiagnosticLevel::Error,                         \
                    "Duplicated type qualifier '" qualifierStr "'.",\
                    qualifier);                                     \
        RELEASE();                                                  \
        return false;                                               \
    } while (0)

// TypeQualifier*
bool Parser::parseTypeQualifierSequence(tokIt &begin,
                                        shared_ptr <Token> &leftmostQualifier,
                                        bool &isConst,
                                        bool &isRestrict,
                                        bool &isVolatile,
                                        bool &isAtomic)
{
    MARK();
    while (auto qualifier = this->parseTypeQualifier(begin)) {
        if (!leftmostQualifier) {
            leftmostQualifier = qualifier;
        }

        switch (qualifier->type) {
            case tokType::CONST_QUALIFIER:
                if (isConst) DUP_QUALIFIER_ERROR("const");
                isConst = true;
                break;
            case tokType::RESTRICT_QUALIFIER:
                if (isRestrict) DUP_QUALIFIER_ERROR("restrict");
                isRestrict = true;
                break;
            case tokType::VOLATILE_QUALIFIER:
                if (isVolatile) DUP_QUALIFIER_ERROR("volatile");
                isVolatile = true;
                break;
            case tokType::ATOMIC_QUALIFIER:
                if (isAtomic) DUP_QUALIFIER_ERROR("atomic");
                isAtomic = true;
                break;

            // This never reached.
            default:
                return false;
        }
    }
#undef DUP_QUALIFIER_ERROR

    return true;
}

#define PARSE_TYPE_QUALIFIER_SEQUENCE()\
    shared_ptr<Token> leftmostQualifier;                    \
    bool isConst = false,                                   \
         isRestrict = false,                                \
         isVolatile = false,                                \
         isAtomic = false;                                  \
    if (!this->parseTypeQualifierSequence(begin,            \
                                          leftmostQualifier,\
                                          isConst,          \
                                          isRestrict,       \
                                          isVolatile,       \
                                          isAtomic))        \
    {                                                       \
        RELEASE_AND_RETURN();                               \
    }

#define SET_QUALIFIER(s)                      \
    s->isConst = isConst;                     \
    s->isRestrict = isRestrict;               \
    s->isVolatile = isVolatile;               \
    s->isAtomic = isAtomic;                   \
    if (leftmostQualifier) {                  \
        s->column = leftmostQualifier->column;\
    }

// TypeSpecifier:
//     TypeQualifier* LeftImplicitTypeSpecifier
//     TypeQualifier* LeftExplicitTypeSpecifier RightTypeOperator(opt)
//     TypeQualifier* LParen TypeSpecifier RParen RightTypeOperator(opt)
shared_ptr<TypeSpecifier> Parser::parseTypeSpecifier(tokIt &begin) {
    MARK();
    PARSE_TYPE_QUALIFIER_SEQUENCE();

    if (auto leftImplicit = this->parseLeftImplicitTypeSpecifier(begin)) {
        SET_QUALIFIER(leftImplicit);
        return leftImplicit;
    }

    auto lParen = CONSUME(L_PAREN);

    shared_ptr<TypeSpecifier> left;
    if (lParen) {
        left = this->parseTypeSpecifier(begin);
    } else {
        left = this->parseLeftExplicitTypeSpecifier(begin);
    }

    if (!left) {
        PARSE_ERROR("Expect left type specifier.");
        RELEASE_AND_RETURN();
    }

    if (lParen) {
        if (auto rParen = CONSUME(R_PAREN)) {
            left->setNewLineAndSpaced(rParen);
        } else {
            this->errorUnmatchedParentheses(lParen);
            RELEASE_AND_RETURN();
        }
    }

    if (auto right = this->parseRightTypeOperator(begin, left)) {
        SET_QUALIFIER(right);
        return right;
    } else {
        SET_QUALIFIER(left);
        return left;
    }
}

// RightTypeOperator:
//     ArrayTypeOperator RightTypeOperator*
//     FunctionTypeOperator RightTypeOperator*
shared_ptr<RightTypeOperator>
Parser::parseRightTypeOperator(tokIt &begin, shared_ptr<TypeSpecifier> left)
{
    bool ok = false;
    shared_ptr<RightTypeOperator> right;
    while (true) {
        if ((right = this->parseArrayTypeOperator(begin, left))
            || (right = this->parseFunctionTypeOperator(begin, left))) {
            left = right;
            ok = true;
        } else if (!ok) {
            PARSE_ERROR("Expect right type operator.");
            return nullptr;
        } else {
            return std::static_pointer_cast<RightTypeOperator>(left);
        }
    }
}

// FunctionTypeOperator:
//     LParen (FunctionParamDecl (Comma FunctionParamDecl)* (Comma Ellipsis)? Comma(opt))? RParen
shared_ptr <FunctionTypeSpecifier>
Parser::parseFunctionTypeOperator(tokIt &begin, shared_ptr <TypeSpecifier> left)
{
    MARK();

    auto lParen = CONSUME(L_PAREN);
    if (!lParen) {
        PARSE_ERROR("Expect '('.");
        return nullptr;
    }

    auto specifier = FunctionTypeSpecifier::init(left);

    shared_ptr<FunctionParamDecl> paramDecl;
    while ((paramDecl = this->parseFunctionParamDecl(begin))) {
        specifier->addParam(paramDecl);

        if (!CONSUME(COMMA)) {
            break;
        } else if (CONSUME(ELLIPSIS)) {
            specifier->hasVariableArg = true;
            CONSUME(COMMA);
            break;
        }
    }

    if (!CONSUME(R_PAREN)) {
        this->errorUnmatchedParentheses(lParen);
        RELEASE_AND_RETURN();
    } else {
        auto parent = this->parseFunctionTypeOperator(begin, specifier);
        if (parent) {
            specifier->parent = parent;
            return parent;
        } else {
            return specifier;
        }
    }
}

// FunctionParamDecl:
//     TypeSpecifier
//     TypeSpecifier Equals Expression
//     TypeSpecifier Identifier
//     TypeSpecifier Identifier Equals Expression
shared_ptr<FunctionParamDecl> Parser::parseFunctionParamDecl(tokIt &begin) {
    MARK();
    auto typeSpecifier = this->parseTypeSpecifier(begin);

    if (!typeSpecifier) {
        RELEASE();
        PARSE_ERROR("Expect type specifier.");
        return nullptr;
    }

    auto id = CONSUME(IDENTIFIER);

    auto defaultValueExpr = CONSUME(EQUALS) ? this->parseExpression(begin)
                                            : nullptr;

    return FunctionParamDecl::init(typeSpecifier, id, defaultValueExpr);
}

// ArrayTypeOperator:
//     (LBracket Expression RBracket)+
shared_ptr <ArrayTypeSpecifier>
Parser::parseArrayTypeOperator(tokIt &begin, shared_ptr <TypeSpecifier> left) {
    MARK();
    auto lBracket = CONSUME(L_BRACKET);
    if (!lBracket) {
        PARSE_ERROR("Expect '['.");
        return nullptr;
    }

    auto expr = this->parseExpression(begin);
    if (!expr) {
        PARSE_ERROR("Expect constant expression inside [...].");
        RELEASE_AND_RETURN();
    }

    if (!CONSUME(R_BRACKET)) {
        this->errorUnmatchedParentheses(lBracket);
        RELEASE_AND_RETURN();
    } else {
        auto specifier = ArrayTypeSpecifier::init(lBracket, expr);

        if (auto children = this->parseArrayTypeOperator(begin, left)) {
            children->parent = specifier;
            specifier->subType = children;
        } else {
            left->parent = specifier;
            specifier->subType = left;
        }

        return specifier;
    }
}

// LeftExplicitTypeSpecifier:
//     TypeQualifier* BasicType
//     TypeQualifier* TypedefSpecifier
//     TypeQualifier* ExplicitPointerTypeSpecifier
//     TypeQualifier* ExplicitAggregateTypeSpecifier
shared_ptr<TypeSpecifier> Parser::parseLeftExplicitTypeSpecifier(tokIt &begin)
{
    MARK();
    PARSE_TYPE_QUALIFIER_SEQUENCE();

    shared_ptr<TypeSpecifier> specifier;
    if ((specifier = this->parseBasicTypeSpecifier(begin))
        || (specifier = this->parseTypedefSpecifier(begin))
        || (specifier = this->parseExplicitPointerTypeSpecifier(begin))
        || (specifier = this->parseExplicitAggregateTypeSpecifier(begin)))
    {
        SET_QUALIFIER(specifier);
        return specifier;
    } else {
        PARSE_ERROR("Except left explicit type specifier.");
        return nullptr;
    }
}

// TypedefSpecifier:
//     Identifier
shared_ptr<TypedefSpecifier> Parser::parseTypedefSpecifier(tokIt &begin) {
    auto ref = CONSUME(IDENTIFIER);
    if (!ref) {
        PARSE_ERROR("Expect typedef reference.");
        return nullptr;
    } else {
        return TypedefSpecifier::init(ref);
    }
}

shared_ptr<BasicTypeSpecifier> Parser::parseBasicTypeSpecifier(tokIt &begin) {
    shared_ptr<Token> typeTok;
    if (false
#define V(tType) || (typeTok = CONSUME(tType))
        FOREACH_BASIC_TYPE_SPECIFIER_TOKTYPE(V)
#undef V
    )
    {
        auto compOrImag = CONSUME(COMPLEX);
        compOrImag = compOrImag ? compOrImag : CONSUME(IMAGINARY);
        switch (typeTok->type) {
            case tokType::FLOAT:
            case tokType::DOUBLE:
            case tokType::L_DOUBLE:
                return compOrImag ? BasicTypeSpecifier::init(typeTok, compOrImag)
                                  : BasicTypeSpecifier::init(typeTok);
            default:
                if (!compOrImag) {
                    return BasicTypeSpecifier::init(typeTok);
                } else {
                    this->errorUnexpectedToken(compOrImag, true);
                    return nullptr;
                }
        }
    } else {
        PARSE_ERROR("Expect basic type name.");
        return nullptr;
    }
}

#define PARSE_POINTER_TIMES() \
    auto pointerAstrisk = CONSUME(TIMES);\
    if (!pointerAstrisk                  \
        || pointerAstrisk->newLine       \
        || pointerAstrisk->spaced)       \
    {                                    \
        return nullptr;                  \
    }

// ExplicitPointerSpecifier:
//     Times(newLine=false, spaced=false) LeftExplicitTypeSpecifier
//     Times(newLine=false, spaced=false) LParen TypeSpecifier RParen
shared_ptr<PointerTypeSpecifier>
Parser::parseExplicitPointerTypeSpecifier(tokIt &begin)
{
    MARK();
    PARSE_POINTER_TIMES();

    shared_ptr<Token> lParen = CONSUME(L_PAREN), rParen;
    shared_ptr<TypeSpecifier> specifier;

    // LParen TypeSpecifier RParen
    if (lParen) {
        specifier = this->parseTypeSpecifier(begin);
        if (!(rParen = CONSUME(R_PAREN))) {
            this->errorUnmatchedParentheses(lParen);
            RELEASE_AND_RETURN();
        } else {
            auto ret = PointerTypeSpecifier::init(pointerAstrisk,
                                                  specifier,
                                                  rParen);
            ret->isImplicit = false;
            return ret;
        }
    // LeftExplicitTypeSpecifier
    } else if ((specifier = this->parseLeftExplicitTypeSpecifier(begin))) {
        auto ret = PointerTypeSpecifier::init(pointerAstrisk, specifier);
        ret->isImplicit = false;
        return ret;
    } else {
        PARSE_ERROR("Except '(' or left explicit type specifier.");
        RELEASE_AND_RETURN();
    }
}

// ImplicitPointerSpecifier:
//     Times(newLine=false, spaced=false) LeftImplicitTypeSpecifier
shared_ptr<PointerTypeSpecifier>
Parser::parseImplicitPointerTypeSpecifier(tokIt &begin) {
    MARK();
    PARSE_POINTER_TIMES();
    
    auto subType = this->parseLeftImplicitTypeSpecifier(begin);
    if (subType) {
        auto ret = PointerTypeSpecifier::init(pointerAstrisk, subType);
        ret->isImplicit = true;
        return ret;
    } else {
        PARSE_ERROR("Expect implicit type specifier.");
        RELEASE_AND_RETURN();
    }
}

#undef PARSE_POINTER_TIMES

// LeftImplicitTypeSpecifier:
//     ImplicitPointerTypeSpecifier
//     ImplicitAggregateTypeSpecifier
shared_ptr<TypeSpecifier> Parser::parseLeftImplicitTypeSpecifier(tokIt &begin) {
    shared_ptr<TypeSpecifier> specifier;
    if ((specifier = this->parseImplicitPointerTypeSpecifier(begin))
        || (specifier = this->parseImplicitAggregateTypeSpecifier(begin))) {
        return specifier;
    } else {
        PARSE_ERROR("Expect pointer specifier or aggregate type specifier.");
        return nullptr;
    }
}

// ExplicitAggregateTypeSpecifier:
//     ExplicitEnumSpecifier
//     ExplicitStructOrUnionSpecifier
shared_ptr<TypeSpecifier>
Parser::parseExplicitAggregateTypeSpecifier(tokIt &begin) {
    shared_ptr<TypeSpecifier> specifier;
    if (!(specifier = std::static_pointer_cast<TypeSpecifier>(
        this->parseExplicitEnumSpecifier(begin)))

        && !(specifier = std::static_pointer_cast<TypeSpecifier>(
        this->parseExplicitStructOrUnionSpecifier(begin))))
    {
        return nullptr;
    } else {
        return specifier;
    }
}

// ImplicitAggregateTypeSpecifier:
//     ImplicitEnumSpecifier
//     ImplicitStructOrUnionSpecifier:
shared_ptr<TypeSpecifier> Parser::parseImplicitAggregateTypeSpecifier(
        tokIt &begin)
{
    shared_ptr<TypeSpecifier> specifier;
    if (!(specifier = std::static_pointer_cast<TypeSpecifier>(
            this->parseImplicitEnumSpecifier(begin)))
        && !(specifier = std::static_pointer_cast<TypeSpecifier>(
            this->parseImplicitStructOrUnionSpecifier(begin))))
    {
        return nullptr;
    } else {
        return specifier;
    }
}

#define DEDUCE_ANONYMITY(token)                             \
    bool isAnonymous;                                       \
    shared_ptr<Token> name;                                 \
    if (!token) {                                           \
        return nullptr;                                     \
    } else {                                                \
        isAnonymous = token->newLine;                       \
        if (!isAnonymous && !(name = CONSUME(IDENTIFIER))) {\
            isAnonymous = true;                             \
        }                                                   \
    }

#define PARSE_STRUCT_OR_UNION()             \
    shared_ptr<Token> structOrUnion;        \
    if (!((structOrUnion = CONSUME(STRUCT)) \
        ||(structOrUnion = CONSUME(UNION))))\
    {                                       \
        return nullptr;                     \
    }                                       \
    DEDUCE_ANONYMITY(structOrUnion)

// ImplicitStructOrUnionSpecifier:
//     StructOrUnion StructOrUnionFieldDeclList
//     StructOrUnion(newLine=false) Identifier(newLine=true) StructOrUnionFieldDecl+(>StructOrUnion.column)
shared_ptr<StructOrUnionSpecifier>
Parser::parseImplicitStructOrUnionSpecifier(tokIt &begin) {
    MARK();
    PARSE_STRUCT_OR_UNION();

    if (!isAnonymous && !name->newLine) {
        PARSE_ERROR("Expect new line.");
        RELEASE_AND_RETURN();
    }

    auto specifier = StructOrUnionSpecifier::init(structOrUnion, name);
    specifier->isImplicit = true;

    if (!this->parseStructOrUnionFieldDeclList(begin, specifier)) {
        RELEASE_AND_RETURN();
    } else {
        return specifier;
    }
}

// ExplicitStructOrUnionSpecifier:
//     StructOrUnion(newLine=false) Identifier(opt, newLine=false) LParen StructOrUnionFieldDeclList RParen
//     StructOrUnion(newLine=false) Identifier
shared_ptr<StructOrUnionSpecifier>
Parser::parseExplicitStructOrUnionSpecifier(tokIt &begin) {
    MARK();
    PARSE_STRUCT_OR_UNION();

    if (!isAnonymous && name->newLine) {
        PARSE_ERROR("Unexpected new line.");
        RELEASE_AND_RETURN();
    }

    if (auto lParen = CONSUME(L_PAREN)) {
        auto specifier = StructOrUnionSpecifier::init(structOrUnion, name);
        specifier->isImplicit = false;

        if (!this->parseStructOrUnionFieldDeclList(begin, specifier)) {
            RELEASE_AND_RETURN();
        }

        if (auto rParen = CONSUME(R_PAREN)) {
            specifier->setNewLineAndSpaced(rParen);
        } else {
            this->errorUnmatchedParentheses(lParen);
            RELEASE_AND_RETURN();
        }

        return specifier;
    } else {
        return StructOrUnionSpecifier::init(structOrUnion, name);
    }
}

// StructOrUnionFieldDecl:
//    Typedef(opt) TypeSpecifier(isImplicit=true) StructOrUnionField(=TypeSpecifier.column) (Comma StructOrUnionField(=TypeSpecifier.column))*
//    Typedef(opt) TypeSpecifier(isImplicit=false) StructOrUnionField (Comma StructOrUnionField)*
bool Parser::parseStructOrUnionFieldDeclList(tokIt &begin,
    shared_ptr<StructOrUnionSpecifier> structOrUnionSpecifier)
{
    unsigned long baseIndent = 0;
    shared_ptr<TypeSpecifier> typeSpecifier;

    while (true) {
        MARK();

        auto storageSpecifier = this->parseStorageSpecifier(begin);

        if (storageSpecifier && storageSpecifier->type != tokType::TYPEDEF) {
            PARSE_ERROR("Illegal storage specifier, 'typedef' is the only leagl"
                    "storage specifier in storage or union field declaration.");
            RELEASE();
            return false;
        }

        if (!(typeSpecifier = this->parseTypeSpecifier(begin))) {
            RELEASE();
            break;
        }

        auto fieldDecl = StructOrUnionFieldDecl::init(
                static_cast<bool>(storageSpecifier), typeSpecifier
        );

        if (structOrUnionSpecifier->isImplicit) {
            if (structOrUnionSpecifier->declList.empty()) {
                baseIndent = typeSpecifier->column;
                if (baseIndent <= structOrUnionSpecifier->minIndent) {
                    RELEASE();
                    PARSE_ERROR("Unexpected indent.");
                    break;
                }
            } else if (typeSpecifier->column < baseIndent) {
                RELEASE();
                PARSE_ERROR("Unexpected indent.");
                break;
            }
        }

        if (!this->parseStructOrUnionFieldList(begin, fieldDecl)) {
            RELEASE();
            break;
        }

        structOrUnionSpecifier->addFieldDecl(fieldDecl);
        fieldDecl->parent = structOrUnionSpecifier;

        auto lastField = fieldDecl->fieldList.back();
        if (lastField->isBitField) {
            // TODO: make it work with literal expr
            if (!lastField->bitLength->newLine) {
                break;
            }
        } else if (!lastField->fieldName->token->newLine) {
            break;
        }

    }

    return !structOrUnionSpecifier->declList.empty();
}

// StructOrUnionField:
//     Identifier Colon Expression
//     Identifier
bool Parser::parseStructOrUnionFieldList(tokIt &begin,
    shared_ptr<StructOrUnionFieldDecl> fieldDecl)
{
    unsigned long baseIndent = 0;

    while (true) {
        shared_ptr<Token> id;
        MARK();
        if (!(id = CONSUME(IDENTIFIER))) {
            break;
        }

        shared_ptr<StructOrUnionField> field;

        if (fieldDecl->fieldType->isBasicTypeSpecifier
            && std::static_pointer_cast<BasicTypeSpecifier>(fieldDecl->fieldType)
               ->token->type != tokType::VOID)
        {
            // Identifier Colon Expression
            MARK();
            shared_ptr<Expression> expr;
            if (CONSUME(COLON) && (expr = this->parseExpression(begin))) {
                field = StructOrUnionField::init(id, expr);
            } else {
                RELEASE();
                field = StructOrUnionField::init(id);
            }
        } else {
            field = StructOrUnionField::init(id);
        }

        fieldDecl->addField(field);

        if (!CONSUME(COMMA)) {
            break;
        }
    }

    return !fieldDecl->fieldList.empty();
}

#define PARSE_ENUM_SPECIFIER_PREFIX()\
    auto enumToken = CONSUME(ENUM);                                     \
    DEDUCE_ANONYMITY(enumToken);                                        \
                                                                        \
    shared_ptr<EnumSpecifier> enumSpecifier;                            \
    if (isAnonymous) {                                                  \
        enumSpecifier = EnumSpecifier::init(enumToken,                  \
                                            shared_ptr<Token>(nullptr));\
    } else {                                                            \
        enumSpecifier = EnumSpecifier::init(enumToken, name);           \
    }

// ImplicitEnumSpecifier:
//     Enum Identifier(Opt, newLine=True) EnumeratorList(>Enum.column)
shared_ptr<EnumSpecifier> Parser::parseImplicitEnumSpecifier(tokIt &begin) {
    MARK();
    PARSE_ENUM_SPECIFIER_PREFIX();

    if (!isAnonymous && !name->newLine) {
        PARSE_ERROR("Expect new line.");
        RELEASE_AND_RETURN();
    }

    enumSpecifier->isImplicit = true;

    if (this->parseEnumeratorList(begin, enumSpecifier)) {
        return enumSpecifier;
    } else {
        PARSE_ERROR("Expect enumerator list.");
        RELEASE_AND_RETURN();
    }
}

// ExplicitEnumSpecifier:
//     Enum Identifier(Opt, newLine=False) LParen EnumeratorList RParen
//     Enum Identifier
shared_ptr<EnumSpecifier> Parser::parseExplicitEnumSpecifier(tokIt &begin) {
    PARSE_ENUM_SPECIFIER_PREFIX();

    enumSpecifier->isImplicit = false;

    MARK();
    shared_ptr<Token> lParen;
    if (!(lParen = CONSUME(L_PAREN))) {
        if (enumSpecifier->isAnonymous) {
            PARSE_ERROR("Expect '('.");
            RELEASE_AND_RETURN();
        } else {
            return enumSpecifier;
        }
    }

    if (!this->parseEnumeratorList(begin, enumSpecifier)) {
        PARSE_ERROR("Expect enumerator list.");
        RELEASE();
        goto parse_end;
    }

    if (auto rParen = CONSUME(R_PAREN)) {
        enumSpecifier->setNewLineAndSpaced(rParen);
    } else {
        this->errorUnmatchedParentheses(lParen);
        RELEASE_AND_RETURN();
    }

parse_end:
    return enumSpecifier;
}

// EnumeratorList:
//     Enumerator (Comma Enumerator)*
bool Parser::parseEnumeratorList(tokIt &begin,
                                 shared_ptr<EnumSpecifier> specifier)
{
    unsigned long baseIndent = 0;

    while (1) {
        MARK();
        auto enumerator = this->parseEnumerator(begin);
        if (!enumerator) {
            break;
        } else {
            if (specifier->isImplicit) {
                if (specifier->enumerators.empty()) {
                    baseIndent = enumerator->column;
                    if (baseIndent <= specifier->minIndent) {
                        RELEASE();
                        PARSE_ERROR("Unexpected indent.");
                        return false;
                    }
                }

                if (enumerator->column < baseIndent) {
                    RELEASE();
                    break;
                }

                specifier->addEnumerator(enumerator);
            } else {
                specifier->addEnumerator(enumerator);
            }
        }

        if (!CONSUME(COMMA)) {
            break;
        }
    }

    return !specifier->enumerators.empty();
}

// Enumerator:
//     Identifier
//     Identifier Equals Expression
shared_ptr<Enumerator> Parser::parseEnumerator(tokIt &begin) {
    auto name = CONSUME(IDENTIFIER);

    if (!name) {
        return nullptr;
    }

    auto id = Identifier::init(name);

    MARK();
    if (!CONSUME(EQUALS)) {
        return Enumerator::init(id);
    } else {
        if (auto expr = this->parseExpression(begin)) {
            return Enumerator::init(id, expr);
        } else {
            RELEASE();
            return Enumerator::init(id);
        }
    }

    return nullptr;
}

// StaticAssertDeclaration:
//     StaticAssert Expression, StringLiteral
//     StaticAssert LParen Expression, StringLiteral RParen
shared_ptr<StaticAssertDecl> Parser::parseStaticAssertDecl(tokIt &begin) {
    auto saTok = CONSUME(STATIC_ASSERT),
         lParen = saTok->spaced ? nullptr : CONSUME(L_PAREN);

    auto testExpr = this->parseExpression(begin);

    if (!CONSUME(COMMA)) {
        FATAL_ERROR("Expect comma between test expression and error message.", CUR_TOK);
    }

    auto errExpr = this->parseExpression(begin);
    if (!errExpr) {
        FATAL_ERROR("Expect expression after ','.", CUR_TOK);
    }

    auto rParen = CONSUME(R_PAREN);
    if (lParen && !rParen) {
        this->errorUnmatchedParentheses(lParen);
    }

    return StaticAssertDecl::init(saTok, testExpr, errExpr, rParen);
}

// Declaration:
//     PragmaDecl
//     StaticAssertDeclaration
shared_ptr<AstNode> Parser::parseExternalDeclaration(tokIt &begin) {
    shared_ptr<AstNode> ret;

    switch (CUR_TOK->type) {
        case tokType::PRE_PROCESS_PRAGMA:
            return this->parsePragmaStmt(begin);

        case tokType::TYPEDEF:
            return this->parseVarDecl(begin);

        case tokType::STRUCT:
        case tokType::UNION:
            ret = this->parseVarDecl(begin);
            return ret ? ret : this->parseTypeSpecifier(begin);

        case tokType::STATIC_ASSERT:
            return this->parseStaticAssertDecl(begin);

        case tokType::PRE_PROCESS_IF:
        case tokType::PRE_PROCESS_IFDEF:
        case tokType::PRE_PROCESS_IFNDEF:
            return this->parsePreProcessCondition(begin, false);

        case tokType::AT_SIGN:
            return this->parseAnnotation(begin);

        default:
            if (auto result = this->parseFunctionDefinition(begin, false)) {
                return result;
            } else if (auto result = this->parseVarDecl(begin)) {
                return result;
            } else {
                return this->parseExpression(begin);
            }
    }
}

} // namespace Parsing
} // namespace ppc
