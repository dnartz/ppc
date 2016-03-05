#include <cstdlib>
#include <memory>
#include <vector>
#include <iostream>

#include <Lexer.h>
#include <Parser.h>
#include <SourceManager.h>
#include <Diagnostic.h>
#include "Ast.h"

namespace ppc {
namespace Parsing {

using std::shared_ptr;
using std::make_shared;
using namespace ppc::Diagnostic;

shared_ptr<TranslationUnit> Parser::parseTranslationUnit() {
    auto begin = this->tokens.begin(),
         end = this->tokens.end();
    auto unit = make_shared<TranslationUnit>(CUR_TOK);

    while (begin != end) {
        if (auto result = this->parseExternalDeclaration(begin)) {
            unit->statements.push_back(result);
            unit->setNewLineAndSpaced(result);

            result->parent = unit;
            result->isExternal = true;
        } else if (begin != end) {
            this->errorUnexpectedToken(CUR_TOK, true);
        }
    }

    return unit;
};

shared_ptr<Token> Parser::consume(tokIt &current, tokType const type) {
    if (current != this->tokens.end() && (*current)->type == type) {
        auto ret = *current;
        current++;

        return ret;
    } else {
        return nullptr;
    }
}

void Parser::errorUnmatchedParentheses(std::shared_ptr<Token> left) {
    this->error(DiagnosticLevel::Error,
                "Unmatched '" + left->value + "'.",
                left,
                true);
}

void Parser::errorUnexpectedToken(shared_ptr<Token> token, bool fatal) {
    this->error(DiagnosticLevel::Error,
                "Unexpected '" + (token ? token->value : "EOF") + "'.",
                token,
                true);
}

void
Parser::errorExpectToken(std::shared_ptr<Token> token,
                         std::string expect,
                         bool fatal)
{
    this->error(
        DiagnosticLevel::Error,
        "Expect '" + expect + "' not '" + (token ? token->value : "EOF") + "'.",
        token,
        true);
}

void Parser::error(DiagnosticLevel lv,
                   std::string strMsg,
                   shared_ptr<Token> tok,
                   bool fatal)
{
    if (tok == nullptr) {
        tok = this->tokens.back();
    }

    auto msg = this->sourceCode->createDiagMseeage(lv, strMsg, tok);
    if (!this-> deepestErr
        || tok->lineno > this->deepestErr->token->lineno
        || (tok->lineno == this->deepestErr->token->lineno
            && tok->column >= this->deepestErr->token->column))
    {
        this->deepestErr = msg;
    }

    if (fatal) {
        msg->print();
        exit(1);
    }
}

Parser::Parser(ppc::SourceCode *sourceCode) :
sourceCode(sourceCode),
tokens(this->sourceCode->tokens)
{ }

} // namespace Parsing
} // namespace ppc