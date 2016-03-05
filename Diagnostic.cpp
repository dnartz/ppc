#include <string>
#include <iostream>

#include <SourceManager.h>
#include <Diagnostic.h>

namespace ppc {

using Diagnostic::DiagnosticMessage;
using Diagnostic::DiagnosticLevel;

inline std::string DiagnosticMessage::DiagnosticLevelToString(
        DiagnosticLevel lv) {
    switch (lv) {
        case DiagnosticLevel::Note:
            return std::string("note");
        case DiagnosticLevel::Warning:
            return std::string("warning");
        case DiagnosticLevel::Error:
            return std::string("error");
    }
}

void DiagnosticMessage::print() {
    std::cout << this->sourceCode->fileName << ':'
    << this->token->lineno << ':' << this->token->column << ": "
    << DiagnosticMessage::DiagnosticLevelToString(this->level)
    << ": " << this->message << std::endl;

    auto len = this->token->codeFragment.length();

    std::string marker;
    std::string::size_type newLinePos = 0,
            nextNewLine;
    do {
        nextNewLine = this->token->codeFragment.find('\n', newLinePos);
        if (nextNewLine == std::string::npos) {
            nextNewLine = len - 1;
        }

        std::cout << std::string(this->token->codeFragment,
                                 newLinePos,
                                 nextNewLine - newLinePos)
        << std::endl;

        if (newLinePos == 0) {
            marker = std::string(token->column - 1, ' ');
            marker += '^';
            marker += std::string(nextNewLine - token->column + 1, '~');
        } else {
            marker = std::string(nextNewLine - newLinePos, '~');
        }

        std::cout << marker << std::endl;

        newLinePos = nextNewLine;
    } while (newLinePos < len - 1);
}

}
