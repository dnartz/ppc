#include <vector>
#include <memory>
#include <iostream>

#include <Ast.h>
#include <Parser.h>

namespace ppc {
namespace Parsing {

shared_ptr <Block> Parser::parseBlock(tokIt &begin, bool parseImport) {
    if (!CUR_TOK) {
        return nullptr;
    }

    auto prevMinIndent = this->blockContext.empty() ? 1
                         : this->blockContext.back()->column;

    auto block = make_shared<Block>(CUR_TOK);
    this->blockContext.push_back(block);

    do {
        if (CUR_TOK->column == prevMinIndent) {
            MARK();

            auto end = this->tokens.end();
            decltype(block->statements) annoAndLabel;
            while (begin != end && CUR_TOK->type == tokType::AT_SIGN) {
                if (auto anno = this->parseAnnotation(begin)) {
                    annoAndLabel.push_back(anno);
                } else {
                    break;
                }
            }
            if (auto label = this->parseGotoLabel(begin)) {
                annoAndLabel.push_back(label);
                for (auto stmt : annoAndLabel) {
                    block->addStatement(stmt);
                }
            } else {
                RELEASE();
                break;
            }
        } else if (CUR_TOK->column != block->column) {
            break;
        }

        auto stmt = this->parseStatement(begin, parseImport);
        if (!stmt) break;

        block->addStatement(stmt);
    } while (CUR_TOK);

    this->blockContext.pop_back();
    if (block->statements.empty()) {
        return nullptr;
    } else {
        return block;
    }
}

shared_ptr <AstNode> Parser::parseStatement(tokIt &begin, bool parseImport) {
    shared_ptr<AstNode> ret;

    switch (CUR_TOK->type) {
        case tokType::SWITCH:
            return this->parseSwitchStmt(begin);

        case tokType::FOR:
            return this->parseForStmt(begin);

        case tokType::WHILE:
            return this->parseWhileStmt(begin);

        case tokType::DO:
            return this->parseDoWhileStmt(begin);

        case tokType::LOOP:
            return this->parseLoopStmt(begin);

        case tokType::ON_SCOPE_EXIT:
            return this->parseOnScopeExit(begin);

        case tokType::GOTO:
            return this->parseGotoStmt(begin);

        case tokType::BREAK:
        case tokType::CONTINUE:
            return this->parseContinueOrBreak(begin);

        case tokType::RETURN:
            return this->parseReturnStmt(begin);

        case tokType::AT_SIGN:
            return this->parseAnnotation(begin);

        case tokType::IF:
        case tokType::UNLESS:
            return this->parseIfExpression(begin);

        case tokType::TYPEDEF:
            return this->parseVarDecl(begin);

        case tokType::STRUCT:
        case tokType::UNION:
            ret = this->parseVarDecl(begin);
            return ret ? ret : this->parseTypeSpecifier(begin);

        case tokType::PRE_PROCESS_IF:
        case tokType::PRE_PROCESS_IFDEF:
        case tokType::PRE_PROCESS_IFNDEF:
            return this->parsePreProcessCondition(begin, false);

        default:
            if (auto label = this->parseGotoLabel(begin)) {
                return label;
            }

            if (auto varDecl = this->parseVarDecl(begin)) {
                return varDecl;
            }

            return this->parseExpression(begin);
    }
}

// OnScpoeExitStmt:
//     OnScopeExit Block(>)
shared_ptr<OnScopeExit> Parser::parseOnScopeExit(tokIt &begin) {
    auto oseTok = CONSUME(ON_SCOPE_EXIT);

    auto block = this->parseBlock(begin, false);
    if (!block) {
        FATAL_ERROR("Expect block after 'on_scope_exit'.", *begin);
    }

    return OnScopeExit::init(oseTok, block);
}

// WhileStmt:
//     While Expression Block(>)
shared_ptr<WhileStmt> Parser::parseWhileStmt(tokIt &begin) {
    auto whileTok = CONSUME(WHILE);

    auto cond = this->parseExpression(begin);
    if (!cond) {
        FATAL_ERROR("Expect condition expression after 'while'", *begin);
    }

    shared_ptr<Block> block;
    if (cond->newLine) {
        block = this->parseBlock(begin, false);
        if (!block) {
            FATAL_ERROR("Expect loop body after condition Expression", *begin);
        }
    } else {
        if (!CONSUME(DO)) {
            FATAL_ERROR("Expect 'do' after 'while'.", CUR_TOK);
        }

        auto expr = this->parseExpression(begin);
        if (!expr) {
            FATAL_ERROR("Expect new line after 'while'.",CUR_TOK);
        }
        block = Block::init(expr);
    }

    return WhileStmt::init(whileTok, cond, block);
}

// DoWhileStmt:
//     Do Block(>) While(=) Expression
shared_ptr<DoWhileStmt> Parser::parseDoWhileStmt(tokIt &begin) {
    auto doTok = CONSUME(DO);
    shared_ptr<Block> block;
    if (doTok->newLine) {
        block = this->parseBlock(begin, false);
    } else {
        auto expr = this->parseExpression(begin);
        if (!expr) {
            FATAL_ERROR("Expect expression after 'do'.", CUR_TOK);
        }

        block = Block::init(expr);
    }

    auto whileTok = CONSUME(WHILE);
    if (!whileTok) {
        this->errorExpectToken(CUR_TOK, "while", true);
    }

    if (whileTok->column != this->blockContext.back()->column) {
        FATAL_ERROR("Unexpected indent.", CUR_TOK);
    }

    auto cond = this->parseExpression(begin);
    if (!cond) {
        FATAL_ERROR("Expect condition expression.", CUR_TOK);
    }

    return DoWhileStmt::init(doTok, block, cond);
}

// LoopStmt:
//    Loop Block(>)
shared_ptr<LoopStmt> Parser::parseLoopStmt(tokIt &begin) {
    auto loopTok = CONSUME(LOOP);

    auto block = this->parseBlock(begin, false);
    if (!block) {
        FATAL_ERROR("Expect block after 'loop'.", CUR_TOK);
    }

    auto ret = LoopStmt::init(loopTok, block);

    return ret;
}

// SwitchLabel:
//     Default
//     (Case | When) IfExpression (Comma IfExpression)*
shared_ptr<SwitchLabel> Parser::parseSwitchLabel(tokIt &begin) {
    shared_ptr<Token> labelTok;
    if (!(labelTok = CONSUME(DEFAULT))
        && !(labelTok = CONSUME(CASE))
        && !(labelTok = CONSUME(WHEN)))
    {
        PARSE_ERROR("Expect 'when' or 'case'.");
        return nullptr;
    }

    auto switchLabel = SwitchLabel::init(labelTok);

    while (labelTok->value != "default" && true) {
        auto expr = this->parseIfExpression(begin);
        if (!expr) {
            FATAL_ERROR(
                "Expect constant expression after '" + labelTok->value + "'.",
                CUR_TOK
            );
        }

        switchLabel->addCondition(expr);

        if (!CONSUME(COMMA)) {
            break;
        }
    }

    if (switchLabel->condList.empty() && labelTok->value != "default") {
        FATAL_ERROR(
                "Expect constant expression after '" + labelTok->value + "'.",
                CUR_TOK
        );
    }

    auto block = this->parseBlock(begin, false);
    if (!block) {
        FATAL_ERROR("Expect block.", *begin);
    }
    block->parent = switchLabel;
    switchLabel->action = block;
    switchLabel->setNewLineAndSpaced(block);

    return switchLabel;
}

// SwitchStmt:
//     Switch Expression(newLine=true) (SwitchLabel Block(>))+ OnScopeExit(opt)
shared_ptr<SwitchStmt> Parser::parseSwitchStmt(tokIt &begin) {
    auto switchTok = CONSUME(SWITCH);

    auto operand = this->parseExpression(begin);
    if (!operand) {
        FATAL_ERROR("Expect expression after switch.", CUR_TOK);
    }

    if (!CUR_TOK) {
        this->errorUnexpectedToken(CUR_TOK, true);
    }
    auto fakeBlock = make_shared<Block>(CUR_TOK);
    this->blockContext.push_back(fakeBlock);

    auto switchStmt = SwitchStmt::init(switchTok, operand);
    while (true) {
        auto switchLabel = this->parseSwitchLabel(begin);
        if (!switchLabel) {
            break;
        }

        if (!switchStmt->minIndent) {
            switchStmt->minIndent = switchLabel->column;
        } else if (switchLabel->column != switchStmt->minIndent) {
            FATAL_ERROR("Unexpected indent.", CUR_TOK);
        }

        switchStmt->addLabel(switchLabel);
    }

    if (switchStmt->labelList.empty()) {
        FATAL_ERROR("Missing 'case' or 'when' statement.", CUR_TOK);
    }

    if (CUR_TOK->type == tokType::ON_SCOPE_EXIT
        && CUR_TOK->column == switchStmt->minIndent)
    {
        auto oseStmt = this->parseOnScopeExit(begin);
        switchStmt->onScopeExit = oseStmt;
        oseStmt->parent = switchStmt;
    }

    this->blockContext.pop_back();
    return switchStmt;
}

// ForStmtCond:
//     When Expression
//     By Expression
//     When Expression By Expression
//     By Expression When Expression
void Parser::parseForStmtCond(tokIt &begin,
                              shared_ptr<Expression> &condExpr,
                              shared_ptr<Expression> &stepExpr)
{
    for (int i = 0;i < 2;++i) {
        auto whenOrBy = CONSUME(WHEN);
        whenOrBy = whenOrBy ? whenOrBy : CONSUME(BY);
        if (!whenOrBy) return;

        auto expr = this->parseExpression(begin);
        if (!expr) {
            FATAL_ERROR(
                    "Expect expression after '" + whenOrBy->value + "'.",
                    *begin
            );
        }

        switch (whenOrBy->type) {
            case tokType::WHEN:
                if (condExpr) {
                    FATAL_ERROR("Duplicated 'when'.", whenOrBy);
                } else {
                    condExpr = expr;
                }
                break;

            case tokType::BY:
                if (stepExpr) {
                    FATAL_ERROR("Duplicated 'by'.", whenOrBy);
                } else {
                    stepExpr = expr;
                }
                break;

            default:
                return;
        }
    }
}

// ForInStmt:
//     For Identifier (Comma Identifier)(opt) In Expression ForStmtCond(opt)
shared_ptr<ForInStmt> Parser::parseForInStmt(tokIt &begin) {
    auto forTok = CONSUME(FOR);
    shared_ptr<Identifier> valueVar, indexVar;

    if (auto id = CONSUME(IDENTIFIER)) {
        valueVar = Identifier::init(id);
    } else {
        FATAL_ERROR("Expect identifier between 'for...in'.", CUR_TOK);
    }

    bool hasComma = false;
    if (CONSUME(COMMA)) {
        hasComma = true;
        if (auto id = CONSUME(IDENTIFIER)) {
            indexVar = Identifier::init(id);
        } else {
            FATAL_ERROR("Expect identifier after ','.", CUR_TOK);
        }
    }

    if (!CONSUME(IN)) {
        this->errorExpectToken(*begin, "in", true);
    }

    auto rangeExpr = this->parseExpression(begin);
    if (!rangeExpr) {
        FATAL_ERROR("Expect expression after 'in'.", CUR_TOK);
    }

    shared_ptr<Expression> condExpr, stepExpr;
    this->parseForStmtCond(begin, condExpr, stepExpr);

    auto last = stepExpr ? stepExpr : condExpr ? condExpr : rangeExpr;
    shared_ptr<Block> block;
    shared_ptr<Token> doTok;
    if (last->newLine) {
        block = this->parseBlock(begin, false);
        if (!block) {
            FATAL_ERROR("Expect loop body.", CUR_TOK);
        }
    } else {
        if (!(doTok = CONSUME(DO))) {
            this->errorExpectToken(CUR_TOK, "do", true);
        }
        auto singleLineStmt = this->parseSingleLineStmt(begin);
        if (!singleLineStmt) {
            FATAL_ERROR("Expect expression after 'do'.", CUR_TOK);
        }

        block = Block::init(singleLineStmt);
    }

    return ForInStmt::init(forTok,
                           valueVar,
                           indexVar,
                           rangeExpr,
                           doTok,
                           block,
                           condExpr,
                           stepExpr);
}

// ForFromStmt:
//     For Expression From Expression To Expression ForStmtCond(opt)
shared_ptr<ForFromStmt> Parser::parseForFromStmt(tokIt &begin) {
    MARK();

    auto forTok = CONSUME(FOR);
    auto itExpr = this->parseExpression(begin);
    if (!itExpr) {
        FATAL_ERROR("Expect expression or identifier after 'for'.", CUR_TOK);
    }

    if (!CONSUME(FROM)) {
        RELEASE_AND_RETURN();
    }

    auto floorExpr = this->parseExpression(begin);
    if (!floorExpr) {
        FATAL_ERROR("Expect expression after 'from'.", CUR_TOK);
    }

    if (!CONSUME(TO)) {
        this->errorExpectToken(*begin, "to", true);
    }

    auto upperExpr = this->parseExpression(begin);
    if (!upperExpr) {
        FATAL_ERROR("Expect expression after 'to'.", CUR_TOK);
    }

    shared_ptr<Token> doTok;
    shared_ptr<Expression> condExpr, stepExpr;
    this->parseForStmtCond(begin, condExpr, stepExpr);

    auto last = stepExpr ? stepExpr : condExpr ? condExpr : upperExpr;
    shared_ptr<Block> block;
    if (last->newLine) {
        block = this->parseBlock(begin, false);
        if (!block) {
            FATAL_ERROR("Expect loop body.", CUR_TOK);
        }
    } else {
        if (!(doTok = CONSUME(DO))) {
            this->errorExpectToken(CUR_TOK, "do", true);
        }
        auto singleLineStmt = this->parseSingleLineStmt(begin);
        if (!singleLineStmt) {
            FATAL_ERROR("Expect expression after 'do'.", CUR_TOK);
        }

        block = Block::init(singleLineStmt);
    }

    return ForFromStmt::init(forTok,
                             itExpr,
                             floorExpr,
                             upperExpr,
                             doTok,
                             block,
                             condExpr,
                             stepExpr);
}

// ForStmt:
//     For Identifier In Expression (When Expr)(opt)
//     For Expr From Expr To Expr (When Expr)(opt) (By Expr)(opt)
shared_ptr<ForStmt> Parser::parseForStmt(tokIt &begin) {
    if (auto forStmt = this->parseForFromStmt(begin)) {
        return forStmt;
    } else {
        return this->parseForInStmt(begin);
    }
}

// ReturnStmt:
//     Return Expression(opt)
shared_ptr<ReturnStmt> Parser::parseReturnStmt(tokIt &begin) {
    auto returnTok = CONSUME(RETURN);
    shared_ptr<Expression> returnExpr;
    if (!returnTok->newLine && !(returnExpr = this->parseExpression(begin))) {
        FATAL_ERROR("Expect Expression after 'return'.", *begin);
    }

    auto ret = ReturnStmt::init(returnTok, returnExpr);

    return ret;
}

// GotoLabel:
//     Identifier(newLine=false) Colon(newLine=true)
shared_ptr<GotoLabel> Parser::parseGotoLabel(tokIt &begin) {
    MARK();
    auto labelName = CONSUME(IDENTIFIER);

    if (!labelName) {
        return nullptr;
    }

    unsigned long expectedCol;
    if (this->blockContext.size() <= 1) {
        expectedCol = 1;
    } else {
        expectedCol = this->blockContext.at(
                this->blockContext.size() - 2
        )->column;
    }

    if (labelName->column != expectedCol) {
        PARSE_ERROR("Invailid indent of goto label.");
        RELEASE_AND_RETURN();
    }

    if (labelName->newLine) {
        PARSE_ERROR("Unexpected new line after '" + labelName->value + "'.");
        RELEASE_AND_RETURN();
    }

    auto colon = CONSUME(COLON);
    if (!colon) {
        PARSE_ERROR("Expect ':' after a label name.");
        RELEASE_AND_RETURN();
    }

    if (!colon->newLine) {
        PARSE_ERROR("Expect new line after goto label.");
        RELEASE_AND_RETURN();
    }

    return make_shared<GotoLabel>(labelName, colon);
}

// GotoStmt:
//     Goto(newLine=false) Identifier
shared_ptr<GotoStmt> Parser::parseGotoStmt(tokIt &begin) {
    auto gotoTok = CONSUME(GOTO),
         labelName = CONSUME(IDENTIFIER);

    if (gotoTok->newLine) {
        FATAL_ERROR("Unexpected new line after 'goto'.", *begin);
    }

    if (!labelName) {
        FATAL_ERROR("Expect label name after 'goto'.", *begin);
    }

    return make_shared<GotoStmt>(gotoTok, labelName);
}

// ContinueOrBreak:
//     Continue
//     Break
shared_ptr<ContinueOrBreak> Parser::parseContinueOrBreak(tokIt &begin) {
    auto continueOrBreak = CONSUME(CONTINUE);
    continueOrBreak = continueOrBreak ? continueOrBreak : CONSUME(BREAK);

    return make_shared<ContinueOrBreak>(continueOrBreak);
}

// PragmaDecl:
//     PreProcessPragma (.*)?(newLine=false) .(newLine = true)
shared_ptr<PragmaDecl> Parser::parsePragmaStmt(tokIt &begin) {
    auto pragmaTok = CONSUME(PRE_PROCESS_PRAGMA);
    if (pragmaTok->newLine) {
        FATAL_ERROR("Unexpected new line after 'pragma'.", CUR_TOK->prev);
    }

    auto pragmaStmt = PragmaDecl::init(pragmaTok);
    auto end = this->tokens.end();
    while (begin != end) {
        pragmaStmt->addToken(*begin);
        begin++;
        if (CUR_TOK->prev->newLine) {
            break;
        }
    }

    if (pragmaStmt->tokens.empty()) {
        FATAL_ERROR("Missing option after 'pragma'.", CUR_TOK);
    }

    return pragmaStmt;
}

// Annotation:
//     AtSign ModuleChain(newLine=true)
//     AtSign ModuleChain(newLine=false, spaced=true) ArugumentsList
//     AtSign ModuleChain(newLine=false, spaced=false) LParen ArugumentsList RParen
shared_ptr<Annotation> Parser::parseAnnotation(tokIt &begin) {
    auto atSign = CONSUME(AT_SIGN);
    auto annoId = CONSUME(IDENTIFIER);

    if (annoId->newLine) {
        return Annotation::init(atSign, annoId);
    }

    if (annoId->spaced) {
        if (auto params = this->parseArgumentExpressionList(begin)) {
            return Annotation::init(atSign, annoId, params);
        } else {
            FATAL_ERROR("Expect arguments or new line after annotation name.", CUR_TOK);
            return nullptr;
        }
    } else if (auto lParan = CONSUME(L_PAREN)) {
        auto params = this->parseArgumentExpressionList(begin);
        auto rParen = CONSUME(R_PAREN);

        if (!rParen) {
            this->errorUnmatchedParentheses(lParan);
        } else if (!rParen->newLine) {
            FATAL_ERROR("Expect new line after ')'.", CUR_TOK);
            return nullptr;
        }

        return Annotation::init(atSign, annoId, params, rParen);
    } else {
        this->errorUnexpectedToken(CUR_TOK, true);
        return nullptr;
    }
}

// SingleLineStmt:
//     ContinueOrBreak
//     Return Stmt
//     GotoStmt
//     WhileStmt(singleLine=true)
//     DoWhileStmt(singleLine=true)
//     ForInStmt(singleLine=true)
//     ForFromStmt(singleLine=true)
//     Expression
shared_ptr<AstNode> Parser::parseSingleLineStmt(tokIt &begin) {
    shared_ptr<WhileStmt> whiltStmt;
    shared_ptr<DoWhileStmt> doWhildStmt;
    shared_ptr<ForStmt> forStmt;
    switch (CUR_TOK->type) {
        case tokType::BREAK:
        case tokType::CONTINUE:
            return this->parseContinueOrBreak(begin);

        case tokType::RETURN:
            return this->parseReturnStmt(begin);

        case tokType::WHILE:
            return this->parseWhileStmt(begin);

        case tokType::DO:
            return this->parseDoWhileStmt(begin);

        case tokType::FOR:
            forStmt = this->parseForStmt(begin);
            if (forStmt && forStmt->singleLine) {
                return forStmt;
            } else {
                FATAL_ERROR("Expect single line statement.", CUR_TOK);
            }

        default:
            return this->parseExpression(begin);
    }
}

// PreProcessIfGroup:
//     PreProcessIf ConstantExpression Block
//     PreProcessIfdef Identifier Block
//     PreProcessIfndef Identifier Block
//
// PreProcessCondition:
//     PreProcessIfGroup (PreProcessElseGroup | PreProcessElifGroup)
shared_ptr <PreProcessCondition>
Parser::parsePreProcessCondition(tokIt &begin, bool allowNoAtSign)
{
    shared_ptr<Token> directiveTok;
    shared_ptr<Expression> expr;
    shared_ptr<Block> block;
    shared_ptr<PreProcessCondition> directive, children;

    switch (CUR_TOK->type) {
        case tokType::IF:
            if (!allowNoAtSign) {
                this->errorUnexpectedToken(CUR_TOK, true);
            }

        case tokType::PRE_PROCESS_IF:
            directiveTok = CUR_TOK;
            begin++;

            if (!(expr = this->parseIfExpression(begin))) {
                FATAL_ERROR("Expect expression.", CUR_TOK);
            }
            break;

        case tokType::NO_AT_PRE_PROCESS_IFDEF:
        case tokType::NO_AT_PRE_PROCESS_IFNDEF:
            if (!allowNoAtSign) {
                this->errorUnexpectedToken(CUR_TOK, true);
            }

        case tokType::PRE_PROCESS_IFDEF:
        case tokType::PRE_PROCESS_IFNDEF:
            directiveTok = CUR_TOK;
            begin++;

            if (CUR_TOK->type != tokType::IDENTIFIER) {
                FATAL_ERROR("Expect identifier after '"
                            + directiveTok->value + "'.", CUR_TOK);
            } else {
                expr = Literal::init(*(begin++));
            }
            break;

        default:
            return nullptr;
    }

    if (!(block = this->parseBlock(begin, this->blockContext.empty()))) {
        FATAL_ERROR("Expect block.", CUR_TOK);
    }

    directive = PreProcessCondition::init(directiveTok, expr, block);
    switch (CUR_TOK->type) {
        case tokType::PRE_PROCESS_ELIF:
            children = this->parsePreProcessElifGroup(begin);
            break;

        case tokType::PRE_PROCESS_ELSE:
            children = this->parsePreProcessElseGroup(begin);
            break;

        default:
            return directive;
    }

    children->parent = directive;
    directive->elseGroup = children;

    return directive;
}

// PreProcessElseGroup:
//     PreProcessElse(newLine=true) Block
//     ProProcessElse(newLine=false) PreProcessCondition
shared_ptr<PreProcessCondition> Parser::parsePreProcessElseGroup(tokIt &begin) {
    auto elseTok = CONSUME(PRE_PROCESS_ELSE);

    if (elseTok->newLine) {
        if (auto block = this->parseBlock(begin, this->blockContext.empty())) {
            return PreProcessCondition::init(elseTok, nullptr, block);
        } else {
            FATAL_ERROR("Expect block after '@else'.", CUR_TOK);
            return nullptr;
        }
    } else {
        shared_ptr<PreProcessCondition> children;
        switch (CUR_TOK->type) {
            case tokType::IF:
            case tokType::PRE_PROCESS_IF:
            case tokType::PRE_PROCESS_IFDEF:
            case tokType::NO_AT_PRE_PROCESS_IFDEF:
            case tokType::PRE_PROCESS_IFNDEF:
            case tokType::NO_AT_PRE_PROCESS_IFNDEF:
                children = this->parsePreProcessCondition(begin, true);
                break;

            default:
                this->errorUnexpectedToken(CUR_TOK, true);
        }

        auto elseDirective =
            PreProcessCondition::init(elseTok, nullptr, children);
        children->parent = elseDirective;

        return elseDirective;
    }
}

// PreProcessElifGroup:
//     PreProcessElif Expression(newLine=true) Block
shared_ptr<PreProcessCondition> Parser::parsePreProcessElifGroup(tokIt &begin) {
    auto elifTok = CONSUME(PRE_PROCESS_ELIF);

    auto cond = this->parseIfExpression(begin);
    if (!cond) {
        FATAL_ERROR("Expect constant expression after '@elif'.", CUR_TOK);
    }

    auto block = this->parseBlock(begin, this->blockContext.empty());
    if (!block) {
        FATAL_ERROR("Expect block after constant expression.", CUR_TOK);
    }

    auto elifDirective = PreProcessCondition::init(elifTok, cond, block);

    shared_ptr<PreProcessCondition> children;
    switch (CUR_TOK->type) {
        case tokType::PRE_PROCESS_ELIF:
            children = this->parsePreProcessElifGroup(begin);
            break;

        case tokType::PRE_PROCESS_ELSE:
            children = this->parsePreProcessElseGroup(begin);
            break;

        default:
            return elifDirective;
    }

    elifDirective->elseGroup = children;
    children->parent = elifDirective;

    return elifDirective;
}

} // namespace Parsing
} // namespace ppc
