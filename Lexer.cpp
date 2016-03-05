#include <ostream>
#include <memory>

#include <SourceManager.h>
#include <Lexer.h>
#include <Diagnostic.h>
namespace ppc {

using ppc::Lexer;
using ppc::Token;
using ppc::Diagnostic::DiagnosticMessage;
using ppc::Diagnostic::DiagnosticLevel;

Lexer::Lexer(SourceCode *sourceCode) :
sourceCode(sourceCode),
code(sourceCode->code),
tokens(sourceCode->tokens)
{
    // Remove BOM
    if (this->code[0] == '\xEF' &&
        this->code[1] == '\xBB' &&
        this->code[2] == '\xBF') {
        this->code.erase(0, 3);
    }
}

void Lexer::token(tokType type,
                  const char *begin,
                  unsigned int count,
                  int lineno,
                  int column) {
    auto fragmentStart = begin - this->code.c_str(),
            fragmentEnd = fragmentStart + count - 1;

    auto codeLen = this->code.length() - 1;

    while (fragmentStart > 0 && this->code[fragmentStart - 1] != '\n') {
        fragmentStart--;
    }

    while (fragmentEnd < codeLen && this->code[fragmentEnd + 1] != '\n') {
        fragmentEnd++;
    }

    auto val = std::string(begin, count),
         fragment = std::string(this->code,
                                fragmentStart,
                                fragmentEnd - fragmentStart + 2);

    auto tokPtr = std::make_shared<Token>(type, val, fragment, lineno, column);
    if (!this->tokens.empty()) {
        this->tokens.back()->next = tokPtr;
        tokPtr->prev = this->tokens.back();
    }
    this->tokens.push_back(tokPtr);
}

void Lexer::error(std::shared_ptr<Token> tok) {
    auto msg = DiagnosticMessage(DiagnosticLevel::Error,
                                 this->sourceCode,
                                 "Unexpected character '" + tok->value + "\'.",
                                 tok);
    msg.print();
}

} // namespace ppc
