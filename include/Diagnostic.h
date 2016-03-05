#ifndef _DIAGNOSTIC_H
#define _DIAGNOSTIC_H

#include <string>
#include <memory>

#include <Lexer.h>

namespace ppc {
namespace Diagnostic {

using ppc::Token;

enum class DiagnosticLevel {
    Note, Warning, Error
};

class SourceCode;

class DiagnosticMessage {
public:
    DiagnosticLevel level;
    ppc::SourceCode *sourceCode;
    std::string message;
    std::shared_ptr<Token> token;

    DiagnosticMessage(DiagnosticLevel lv,
                      ppc::SourceCode *sourceCode,
                      std::string msg,
                      std::shared_ptr<Token> tok) :
    level(lv),
    message(msg),
    sourceCode(sourceCode),
    token(tok)
    { }

    static inline std::string DiagnosticLevelToString(DiagnosticLevel lv);

    void print();
};

} // namespace diagnostic
} // namespace ppc

#endif
