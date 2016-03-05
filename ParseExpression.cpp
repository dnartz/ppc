#include <memory>

#include <Ast.h>
#include <Parser.h>

namespace ppc {
namespace Parsing {

using std::shared_ptr;
using std::make_shared;

// Literal:
//     IntConstDec
//     IntConstHex
//     IntConstOct
//     IntConstBin
//     FloatConst
//     FloatConstHex
//     CharConst
//     WCharConst
//     StringConst
//     WStringConst
//     HereStringConst
//     HereWStringConst
//     True
//     False
//     Null
//     __FUNC__
//     __PRETTY_FUNC__
shared_ptr<Expression> Parser::parseLiteral(tokIt &begin) {
    shared_ptr<Token> literal;
    if (false
#define V(tType) || (literal = CONSUME(tType))
        FOREACH_BASIC_TYPE_LITERAL_TOKTYPE(V)
#undef V
    )
    {
        return Literal::init(literal);
    } else {
        return nullptr;
    }
}

// PrimaryExpression:
//     LParen Expression RParen
//     Literal
//     CompoundLiteral
//     Lambda
shared_ptr<Expression> Parser::parsePrimaryExpression(tokIt &begin) {
    shared_ptr<Token> lParen, rParen;
    shared_ptr<Expression> expr;

    MARK();

    if (auto compoundLiteral = this->parseCompoundLiteral(begin)) {
        return compoundLiteral;
    }

    if (auto Lambda = this->parseFunctionDefinition(begin, true)) {
        return Lambda;
    }

    if ((lParen = CONSUME(L_PAREN))
        && (expr = this->parseExpression(begin))
        && (rParen = CONSUME(R_PAREN)))
    {
        return BraceExpression::init(lParen, expr, rParen);
    } else if (lParen && !rParen){
        this->errorUnmatchedParentheses(lParen);
    } else {
        RELEASE();
    }

    if ((expr = this->parseLiteral(begin))) {
        return expr;
    } else {
        PARSE_ERROR("Expect primary expression.");
        return nullptr;
    }
}

// UnaryOptr:
//     & | * | + | - | ~ | not
shared_ptr<Token> Parser::parseUnaryOptr(tokIt &begin) {
    shared_ptr<Token> optr;
    if ((optr = CONSUME(BITWISE_AND))
        || (optr = CONSUME(PLUS))
        || (optr = CONSUME(MINUS))
        || (optr = CONSUME(BITWISE_NOT))
        || (optr = CONSUME(TIMES))
        || (optr = CONSUME(LOGIC_NOT)))
    {
        if (optr->newLine || optr->spaced)
        {
            if (optr->type != tokType::PLUS
                && optr->type != tokType::MINUS
                && optr->type != tokType::TIMES)
            {
                FATAL_ERROR(
                    "Unexpected new line or space after unary operator.",
                    CUR_TOK->prev
                );
            }
            return nullptr;
        } else {
            return optr;
        }
    } else {
        return nullptr;
    }
}

// CompoundLiteral:
//     LBracket InitializerList RBracket
//     LBrace InitializerList RBrace
//     InitializerList
shared_ptr<CompoundLiteral> Parser::parseCompoundLiteral(tokIt &begin) {
    MARK();

    auto lBracket = CONSUME(L_BRACKET);
    lBracket = lBracket ? lBracket : CONSUME(L_BRACE);

    auto result = this->parseInitializerList(begin, lBracket);

    shared_ptr<Token> match;
    if (lBracket) {
        match = lBracket->type == tokType::L_BRACE ? CONSUME(R_BRACE)
                                                   : CONSUME(R_BRACKET);
    }
    if (!result) {
        if (lBracket && match) {
            result = CompoundLiteral::init(lBracket);
            result->setNewLineAndSpaced(match);
        } else {
            PARSE_ERROR("Expect compound list.");
            RELEASE_AND_RETURN();
        }
    }

    if (lBracket) {
        if (!match) {
            this->errorUnmatchedParentheses(lBracket);
            RELEASE_AND_RETURN();
        } else {
            result->setNewLineAndSpaced(match);
        }
    }

    return result;
}

// InitializerList:
//     Initializer Comma InitializerList
//     Initializer(newLine=true) InitializerList
//
// Initializer:
//     ExprKeyInitializer
//     IdentifierKeyInitializer
//     Expression
shared_ptr <CompoundLiteral>
Parser::parseInitializerList(tokIt &begin, shared_ptr <Token> left) {
    auto literal = left ? CompoundLiteral::init(left) : CompoundLiteral::init();

#define V(parseAct, checkIndent)                                 \
    while (true) {                                               \
        MARK();                                                  \
        auto kv = this->parseAct(begin);                         \
        if (kv) {                                                \
            if (!checkIndent                                     \
                || !literal->minIndent                           \
                || kv->first->column == literal->minIndent)      \
            {                                                    \
                literal->addKvPair(kv);                          \
            } else if (kv->first->column != literal->minIndent) {\
                RELEASE();                                       \
                return literal;                                  \
            } else {                                             \
                literal->addKvPair(kv);                          \
            }                                                    \
        } else if (!literal->initializerList.empty()) {          \
            return literal;                                      \
        } else {                                                 \
            break;                                               \
        }                                                        \
        auto comma = CONSUME(COMMA);                             \
        bool wrongCommaPos =                                     \
            !left && comma && comma->column < literal->minIndent;\
        if (!comma && !kv->second->newLine || wrongCommaPos) {   \
            if (wrongCommaPos) begin--;                          \
            if (literal->initializerList.empty()) {              \
                break;                                           \
            } else {                                             \
                return literal;                                  \
            }                                                    \
        }                                                        \
    }

    if (left && left->type == tokType::L_BRACKET) {
        V(parseExprKeyInitializer, true)
    } else /* if no left bracket or has left brace.*/ {
        V(parseIdentifierKeyInitializer, true)
    }

    if (left) {
        V(parseExprInitializer, false)
    }
#undef V

    return nullptr;
}

// ExprKeyInitializer:
//     LParen Expression RParen Colon Expression
shared_ptr<CompoundLiteral::kvPair>
Parser::parseExprKeyInitializer(tokIt &begin) {
    MARK();

    auto lParen  = CONSUME(L_PAREN);
    if (!lParen) {
        PARSE_ERROR("Expect left parentheses in expression key initializer.");
        RELEASE_AND_RETURN();
    }

    auto keyExpr = this->parseExpression(begin);
    if (!keyExpr) {
        PARSE_ERROR("Expect expression.");
        RELEASE_AND_RETURN();
    }

    auto rParen  = CONSUME(R_PAREN),
         colon   = CONSUME(COLON);
    if (!rParen || !colon) {
        PARSE_ERROR("Expect right parentheses followed by colon.");
        RELEASE_AND_RETURN();
    }

    auto valExpr = this->parseExpression(begin);

    if (valExpr) {
        return make_shared<CompoundLiteral::kvPair>(
                BraceExpression::init(lParen, keyExpr, rParen),
                valExpr
        );
    } else {
        RELEASE_AND_RETURN();
    }
}

// IdentifierKeyInitializer:
//     Identifier Colon Expression
shared_ptr<CompoundLiteral::kvPair>
Parser::parseIdentifierKeyInitializer(tokIt &begin) {
    MARK();

    auto id = CONSUME(IDENTIFIER);
    if (!id) {
        PARSE_ERROR("Expect identifier.");
        RELEASE_AND_RETURN();
    }
    auto colon = CONSUME(COLON);
    if (!colon) {
        PARSE_ERROR("Expect colon.");
        RELEASE_AND_RETURN();
    }
    auto valExpr = this->parseExpression(begin);

    if (valExpr) {
        return make_shared<CompoundLiteral::kvPair>(
                Identifier::init(id),
                valExpr
        );
    } else {
        RELEASE_AND_RETURN();
    }
}

// ParseExpressionInitializer:
//     Expression
shared_ptr<CompoundLiteral::kvPair> Parser::parseExprInitializer(tokIt &begin) {
    auto expr = this->parseExpression(begin);

    if (expr) {
        return make_shared<CompoundLiteral::kvPair>(nullptr, expr);
    } else {
        return nullptr;
    }
}

// PostfixOptr:
//     LBracket Expression RBracket
//     LParen ArgumentExpressionList RParen
//     Query Period Identifier
//     Query Arrow Identifier
//     PlusPlus
//     MinusMinus
shared_ptr<Expression>
Parser::parsePostfixOptr(tokIt &begin, shared_ptr <Expression> prefix) {
    MARK();
    shared_ptr<Identifier> idNode;
    shared_ptr<Token> query, left, right, id;
    shared_ptr<Expression> expr, ret;
    shared_ptr<ArgumentExpressionList> arguments;

    if (!CUR_TOK) {
        return nullptr;
    }

    switch (CUR_TOK->type) {
        case tokType::L_BRACKET:
            if (prefix->newLine) {
                return prefix;
            }

            if (!(left = CONSUME(L_BRACKET))) {
                PARSE_ERROR("Expect left bracket.");
                RELEASE_AND_RETURN();
            }

            expr = this->parseArgumentExpressionList(begin);
            if (!expr) {
                PARSE_ERROR("Expect subscript expression");
                RELEASE_AND_RETURN();
            }

            right = CONSUME(R_BRACKET);

            if (right) {
                return ArrayAccess::init(prefix, expr, right);
            } else {
                this->errorUnmatchedParentheses(left);

                RELEASE();
                return prefix;
            }

        case tokType::L_PAREN:
            if (prefix->newLine || prefix->spaced) {
                return prefix;
            }

            left = CONSUME(L_PAREN);

            arguments = this->parseArgumentExpressionList(begin);

            right = CONSUME(R_PAREN);
            if (!right) {
                this->errorUnmatchedParentheses(left);
                RELEASE_AND_RETURN();
            }

            if (arguments) {
                return UndeterminedCall::init(prefix, arguments, right);
            } else {
                if (!arguments) {
                    PARSE_ERROR("Expect argument list.");
                } else {
                    this->errorUnmatchedParentheses(right);
                }

                RELEASE();
                return prefix;
            }

        case tokType::QUERY:
        case tokType::PERIOD:
        case tokType::ARROW:
            query = CONSUME(QUERY);
            left = (left = CONSUME(PERIOD)) ? left : CONSUME(ARROW);

            if (!left || (left->type == tokType::PERIOD && query)) {
                PARSE_ERROR("Unexpected token.");
                RELEASE_AND_RETURN();
            }

            id = CONSUME(IDENTIFIER);

            if (id) {
                return StructOrUnionFieldAccess::init(prefix,
                                                      query,
                                                      left,
                                                      idNode);
            }

            PARSE_ERROR("Expect field name.");
            RELEASE();
            return prefix;

        case tokType::PLUS_PLUS:
        case tokType::MINUS_MINUS:
            if (prefix->newLine || prefix->spaced) {
                return prefix;
            }

            left = (left = CONSUME(PLUS_PLUS)) ? left : CONSUME(MINUS_MINUS);
            return UnaryArithmeticExpression::init(prefix, left);

        default:
            return prefix;
    }
}

// PostfixExpression:
//     PrimaryExpression PostfixOptr+(newLine=false, spaced=true) ArgumentExpressionList
//     PrimaryExpression PostfixOptr*
shared_ptr<Expression> Parser::parsePostfixExpression(tokIt &begin) {
    auto left = this->parsePrimaryExpression(begin);
    if (!left) {
        return nullptr;
    }

    while (true) {
        auto right = this->parsePostfixOptr(begin, left);
        if (right && right != left) {
            left = right;
        } else {
            break;
        }
    }

    if (!left->newLine && left->spaced) {
        auto right = this->parseArgumentExpressionList(begin);
        if (right && !right->arguments.empty()) {
            return UndeterminedCall::init(left, right);
        }
    }

    return left;
}

// UnaryExpression:
//     PostfixExpression
//     PlusPlus UnaryExpression
//     MinusMinus UnaryExpression
//     UnaryOperator CastExpression
//     OffsetOf TypeSpecifier Comma Identifier
//     OffsetOf LParen TypeSpecifier Comma Identifier RParen
//     SizeOf UnaryExpression
//     SizeOf TypeSpecifier
//     Longjmp UnaryExpression Comma Expression
//     Longjmp Lparen UnaryExpression Comma Expression RParen
//     VaArg UnaryExpression Comma TypeSpecifier(opt)
//     VaArg LParen UnaryExpression Comma TypeSpecifier(opt) RParen
//     Setjmp UnaryExpression
//     VaStart UnaryExpression
//     VaCopy UnaryExpression
//     VaEnd UnaryExpression
//     AlignOf TypeSpecifier
shared_ptr<Expression> Parser::parseUnaryExpression(tokIt &begin) {
    if (auto expr = this->parsePostfixExpression(begin)) {
        return expr;
    }

    shared_ptr<Token> optr, lParen, rParen, fieldName;
    shared_ptr<TypeSpecifier> typeSpecifier;

    if ((!(optr = CONSUME(PLUS_PLUS))
        && !(optr = CONSUME(MINUS_MINUS))
        && !(optr = CONSUME(OFFSETOF))
        && !(optr = CONSUME(SIZEOF))
        && !(optr = CONSUME(ALIGNOF))
        && !(optr = CONSUME(LONGJMP))
        && !(optr = CONSUME(SETJMP))
        && !(optr = this->parseUnaryOptr(begin)))
        || optr->newLine)
    {
        return nullptr;
    }

    switch (optr->type) {
        case tokType::OFFSETOF:
            if (!optr->spaced) {
                lParen = CONSUME(L_PAREN);
            }

            if (!(typeSpecifier = this->parseTypeSpecifier(begin))) {
                FATAL_ERROR("Expect type specifier after 'offsetof'.", CUR_TOK);
            }

            if (!CONSUME(COMMA)) {
                FATAL_ERROR("Expect comma between the type specifier and the field name.", CUR_TOK);
            }

            if (!(fieldName = CONSUME(IDENTIFIER))) {
                FATAL_ERROR("Expect field name in the second argument.", CUR_TOK);
            }

            if (lParen && !(rParen = CONSUME(R_PAREN))) {
                this->errorUnmatchedParentheses(lParen);
            }

            return OffsetOfExpression::init(optr,
                                            typeSpecifier,
                                            fieldName,
                                            rParen);

        case tokType::SIZEOF:
            if (auto expr = this->parseUnaryExpression(begin)) {
                return SizeOfExpression::init(optr, expr);
            } else if (auto typeSpecifier = this->parseTypeSpecifier(begin)) {
                return SizeOfExpression::init(optr, typeSpecifier);
            } else {
                FATAL_ERROR("Expect unary expression or type specifier after 'sizeof'.", CUR_TOK);
            }

        case tokType::ALIGNOF:
            if (auto typeSpecifier = this->parseTypeSpecifier(begin)) {
                return AlignOfExpression::init(optr, typeSpecifier);
            } else {
                FATAL_ERROR("Expect type specifier after 'alignof'.", CUR_TOK);
            }

        case tokType::VA_ARG:
            if (!optr->spaced) {
                lParen = CONSUME(L_PAREN);
            }

            if (auto expr = this->parseUnaryExpression(begin)) {
                if (CONSUME(COMMA)) {
                    typeSpecifier = this->parseTypeSpecifier(begin);
                }

                rParen = CONSUME(R_PAREN);

                if (lParen && !rParen) {
                    this->errorUnmatchedParentheses(lParen);
                }

                return VaArgExpression::init(optr, expr, typeSpecifier, rParen);
            } else {
                FATAL_ERROR("Expect left value after 'va_arg'.", CUR_TOK);
            }

        case tokType::LONGJMP:
            if (!optr->spaced) {
                lParen = CONSUME(L_PAREN);
            }

            if (auto expr = this->parseUnaryExpression(begin)) {
                if (!CONSUME(COMMA)) {
                    FATAL_ERROR("Expect comma between lvalue and int val", CUR_TOK);
                }

                auto intVal = this->parseExpression(begin);
                if (!intVal) {
                    FATAL_ERROR("Expect expression.", CUR_TOK);
                }

                if (lParen && !(rParen = CONSUME(R_PAREN))) {
                    this->errorUnmatchedParentheses(lParen);
                }

                return LongjmpExpression::init(optr, expr, intVal, rParen);
            } else {
                FATAL_ERROR("Expect lvalue after longjmp.", CUR_TOK);
            }

        case tokType::SETJMP:
        case tokType::VA_START:
        case tokType::VA_COPY:
        case tokType::VA_END:
            if (!optr->next
                || (optr->next->type != tokType::L_PAREN && !optr->spaced))
            {
                FATAL_ERROR("Expect space or left parentheses.", CUR_TOK);
            }

            if (auto expr = this->parseUnaryExpression(begin)) {
                return UnaryArithmeticExpression::init(optr, expr);
            } else {
                FATAL_ERROR("Expect lvalue after '" + optr->value + "'.", CUR_TOK);
            }

        case tokType::PLUS_PLUS:
        case tokType::MINUS_MINUS:
            if (optr->spaced || optr->newLine) {
                FATAL_ERROR("Unexpected space or new line.", CUR_TOK);
            }

            if (auto expr = this->parseUnaryExpression(begin)) {
                return UnaryArithmeticExpression::init(optr, expr);
            } else {
                FATAL_ERROR("Expect unary expression after '" + optr->value + "'.", CUR_TOK);
            }

        // UnaryOperator
        default:
            if (auto expr = this->parseCastExpression(begin)) {
                return UnaryArithmeticExpression::init(optr, expr);
            } else {
                begin--;
                return nullptr;
            }
    }
}

// CastExpression:
//     UnaryExpression
//     LParen TypeSpecifier RParen CastExpression
shared_ptr<Expression> Parser::parseCastExpression(tokIt &begin) {
    auto expr = this->parseUnaryExpression(begin);

    if (expr) {
        return expr;
    }

    MARK();

    auto lParen = CONSUME(L_PAREN);
    if (!lParen) {
        PARSE_ERROR("Expect left parenthess in the head of cast expression.");
        RELEASE_AND_RETURN();
    }

    auto typeSpecifier = this->parseTypeSpecifier(begin);
    if (!typeSpecifier) {
        PARSE_ERROR("Expect type specifier.");
        RELEASE_AND_RETURN();
    }

    auto rParen = CONSUME(R_PAREN);
    if (!rParen) {
        this->errorUnmatchedParentheses(lParen);
        RELEASE_AND_RETURN();
    }

    expr = this->parseCastExpression(begin);

    if (expr) {
        return CastExpression::init(lParen, typeSpecifier, expr);
    } else {
        PARSE_ERROR("Expect expression.");
        RELEASE_AND_RETURN();
    }
}

int binOptrPre(shared_ptr<Token> tok) {
    switch (tok->type) {
        case tokType::TIMES:
        case tokType::DIVIDE:
        case tokType::MOD:
            return 3;

        case tokType::PLUS:
        case tokType::MINUS:
            return 4;

        case tokType::L_SHIFT:
        case tokType::R_SHIFT:
            return 5;

        case tokType::GT:
        case tokType::GTE:
        case tokType::LT:
        case tokType::LTE:
            return 6;

        case tokType::LOGIC_IS:
        case tokType::LOGIC_ISNT:
            return 7;

        case tokType::BITWISE_AND:
            return 8;

        case tokType::BITWISE_XOR:
            return 9;

        case tokType::BITWISE_OR:
            return 10;

        case tokType::LOGIC_AND:
            return 11;

        case tokType::LOGIC_OR:
            return 12;

        default:
            return -1;
    }
}

// BinaryArithmeticExpression:
//     CastExpression (BinaryArithmeticOptr CastExpression)+
shared_ptr<Expression> Parser::parseBinaryArithmeticExpression(tokIt &begin) {
    shared_ptr<BinaryExpression> result;
    shared_ptr<Expression> ePrev, eCurrent;
    shared_ptr<Token> oPrev, oCurrent;

    if (!(eCurrent = this->parseCastExpression(begin))) {
        PARSE_ERROR("Expect expression.");
        return nullptr;
    }

    while (true) {
        MARK();

        oPrev = oCurrent;
#define V(tType) && !(oCurrent = CONSUME(tType))
        if (true
            FOREACH_BINARY_ARITHMETIC_OPERATOR_TOKTYPE(V)
           )
#undef V
        {
            return result ? result : eCurrent;
        }

        ePrev = eCurrent;
        if (!(eCurrent = this->parseCastExpression(begin))) {
            RELEASE();
            return result ? result : eCurrent;
        }

        if (!result && ePrev) {
            result = BinaryExpression::init(ePrev, oCurrent, eCurrent);
        } else if (result) {
            if (binOptrPre(oPrev) <= binOptrPre(oCurrent)) {
                result = BinaryExpression::init(result, oCurrent, eCurrent);
            } else {
                auto child = BinaryExpression::init(result->right,
                                                    oCurrent,
                                                    eCurrent);
                result->right = child;
                child->parent = result;
            }
        }

    }
}

// IfExpression:
//     BinaryArithmeticExpression
//     IfOrUnless Unlikely(opt) Expression Then SingleLineStmt (Else SingleLineStmt)(opt)
//     IfOrUnless Unlikely(opt) Expression(newline=true) Block (Else(=) Block)(opt)
shared_ptr<Expression> Parser::parseIfExpression(tokIt &begin) {
    auto bExpr = this->parseBinaryArithmeticExpression(begin);
    if (bExpr) return bExpr;

    shared_ptr<Token> ifOrUnless;
    if (!(ifOrUnless = CONSUME(IF))
        && !(ifOrUnless = CONSUME(UNLESS)))
    {
        return nullptr;
    }

    auto unlikelyTok = CONSUME(UNLIKELY);

    auto condExpr = this->parseExpression(begin);

    auto thenTok = CONSUME(THEN);

    if (thenTok) {
        if (ifOrUnless->newLine
            || (unlikelyTok && unlikelyTok->newLine)
            || condExpr->newLine)
        {
            FATAL_ERROR("Unexpected new line.", CUR_TOK);
        }

        shared_ptr<AstNode> trueStmt, falseStmt;
        if (!(trueStmt = this->parseSingleLineStmt(begin))) {
            FATAL_ERROR("Expect expression after 'then'.", CUR_TOK);
        }

        auto elseTok = CONSUME(ELSE);
        if (elseTok) {
            if (elseTok->newLine) {
                FATAL_ERROR("Unexpected new line after '"
                            + elseTok->value
                            + "'.",
                            CUR_TOK);
            }

            falseStmt = this->parseSingleLineStmt(begin);
            if (!falseStmt) {
                FATAL_ERROR("Expect expression after '"
                            + elseTok->value
                            + "'.",
                            CUR_TOK);
            }
        }

        return IfExpression::init(ifOrUnless,
                                  unlikelyTok,
                                  condExpr,
                                  Block::init(trueStmt),
                                  falseStmt ? Block::init(falseStmt)
                                            : nullptr);
    } else {
        if (!condExpr->newLine) {
            FATAL_ERROR("Expect new line or 'then'.", CUR_TOK);
        }

        shared_ptr<Block> trueBlock = this->parseBlock(begin, false),
                          falseBlock;
        auto elseTok = CONSUME(ELSE);
        if (!elseTok && !trueBlock) {
            FATAL_ERROR("Expect expression.", CUR_TOK);
        }

        if (elseTok) {
            if (elseTok->column < (this->blockContext.size() > 0
                                   ? this->blockContext.back()->column
                                   : 0))
            {
                FATAL_ERROR("Unexpected indent.", CUR_TOK);
            }

            if (elseTok->newLine) {
                falseBlock = this->parseBlock(begin, false);
            } else if (auto falseExpr = this->parseExpression(begin)) {
                falseBlock = Block::init(falseExpr);
            }

            if (!falseBlock) {
                FATAL_ERROR("Expect expression.", CUR_TOK);
            }
        }

        return IfExpression::init(ifOrUnless,
                                  unlikelyTok,
                                  condExpr,
                                  trueBlock,
                                  falseBlock);
    }
}

// AssignmentExpression:
//     IfExpression
//     UnaryExpression AssignmentOperator AssignmentExpression
//
// AssignmentOperator:
//     = | *= | /= | %= | += | -= | <<= | >>= | &= | ^= | |= | ?= | :=
shared_ptr<Expression> Parser::parseAssignmentExpression(tokIt &begin) {
    MARK();

    auto first = this->parseUnaryExpression(begin);
    if (!first) {
        return this->parseIfExpression(begin);
    }

    shared_ptr<Token> optr;
    if (true
#define V(tType) && !(optr = CONSUME(tType))
        FOREACH_ASSIGNMENT_OPERATOR_TOKTYPE(V)
#undef V
    ) {
        RELEASE();
        return this->parseIfExpression(begin);
    } else {
        auto right = this->parseAssignmentExpression(begin);
        if (!right) {
            RELEASE();
            return this->parseIfExpression(begin);
        } else {
            return BinaryExpression::init(first, optr, right);
        }
    }
}

// ArgumentExpression:
//     AssignmentExpression(newLine=false) (Comma AssignmentExpression(newLine=false))*
shared_ptr<ArgumentExpressionList>
Parser::parseArgumentExpressionList(tokIt &begin) {
    auto arguments = ArgumentExpressionList::init();
    while (true) {
        auto expr = this->parseAssignmentExpression(begin);
        if (expr) {
            arguments->addArguments(expr);
        } else {
            break;
        }

        shared_ptr<Token> comma;
        if (!(comma = CONSUME(COMMA))) {
            break;
        } else if (expr->newLine
                   && comma->column < this->blockContext.back()->column)
        {
            begin--;
            break;
        }
    }

    return arguments;
}

// FunctionDefinition:
//     TypeSpecifier Arrow Block
//     TypeSpecifier Identifier LParen FunctionParamDeclList RParen Arrow Block
shared_ptr<FunctionDefinition>
Parser::parseFunctionDefinition(tokIt &begin, bool isAnonymous) {
    MARK();

    shared_ptr<Token> inlineTok, noReturnTok;
    this->parseFunctionQualifier(begin, inlineTok, noReturnTok);

    bool hasVariableArg = false;
    auto typeSpecifier = this->parseTypeSpecifier(begin);

    if (!typeSpecifier) {
        RELEASE_AND_RETURN();
    }

    shared_ptr<Token> arrow;
    shared_ptr<Identifier> functionName;
    shared_ptr<Block> functionBody;
    std::vector<shared_ptr<FunctionParamDecl>> paramDeclList;
    shared_ptr<TypeSpecifier> returnType;

    if ((arrow = CONSUME(ARROW)) && isAnonymous) {
        if (inlineTok) {
            FATAL_ERROR("Inline is not allowed in lambda function.", inlineTok);
        }

        if (noReturnTok) {
            FATAL_ERROR("Noreturn is not allowed in lambda function.",
                        noReturnTok);
        }

        auto functionTypeSpecifier =
                std::dynamic_pointer_cast<FunctionTypeSpecifier>(typeSpecifier);

        if (!functionTypeSpecifier) {
            FATAL_ERROR("Expect function type specifier.", *__mark);
        }

        returnType = functionTypeSpecifier->subType;
        hasVariableArg = functionTypeSpecifier->hasVariableArg;
        paramDeclList = functionTypeSpecifier->paramDeclList;
    } else if (!isAnonymous) {
        functionName = CONSUME(IDENTIFIER) ? Identifier::init(*(begin-1))
                                           : nullptr;

        if (!functionName) {
            FATAL_ERROR("Expect function name.", CUR_TOK);
        }

        returnType = typeSpecifier;

        auto lParen = CONSUME(L_PAREN);
        if (!lParen) {
            PARSE_ERROR("Expect left parenthess of argument list.");
            RELEASE_AND_RETURN();
        }

        while (true) {
            auto param = this->parseFunctionParamDecl(begin);

            if (!param) {
                if (CONSUME(ELLIPSIS)) {
                    hasVariableArg = true;
                    break;
                } else if (paramDeclList.empty()) {
                    break;
                } else{
                    this->errorUnexpectedToken(*(begin - 1), true);
                }
            }
            paramDeclList.push_back(param);

            if (!CONSUME(COMMA)) {
                break;
            }
        }

        if (!CONSUME(R_PAREN)) {
            this->errorUnmatchedParentheses(lParen);
        }

        if (!(arrow = CONSUME(ARROW))) {
            this->errorExpectToken(CUR_TOK, "->", true);
        }
    } else {
        PARSE_ERROR("Invalid function definition");
        RELEASE_AND_RETURN();
    }

    bool needDefaultValue = false;
    for (auto param : paramDeclList) {
        if (!param->paramName) {
            FATAL_ERROR("Expect param name.", *begin);
        }

        if (needDefaultValue && !param->defaultValue) {
            FATAL_ERROR("Expect default value.", param->paramName->token->next);
        }

        if (param->defaultValue) {
            needDefaultValue = true;
        }
    }

    if (!arrow->newLine) {
        auto singleStmt = this->parseSingleLineStmt(begin);
        if (!singleStmt) {
            FATAL_ERROR("Expect single line statement after function arrow.", CUR_TOK);
        }

        functionBody = Block::init(singleStmt);
    } else if (!(functionBody = this->parseBlock(begin, false))) {
        FATAL_ERROR("Expect function body.", *begin);
    }

    return FunctionDefinition::init(inlineTok,
                                    noReturnTok,
                                    returnType,
                                    functionName,
                                    paramDeclList,
                                    hasVariableArg,
                                    functionBody);

}

} // namespace Parsing
} // namespace ppc
