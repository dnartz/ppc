#ifndef _SOURCE_MANAGER_H
#define _SOURCE_MANAGER_H

#include <string>
#include <vector>
#include <memory>

#include <Ast.h>
#include <Lexer.h>
#include <Parser.h>
#include <Diagnostic.h>
#include <AstVisitor.h>

namespace ppc {

using std::string;
using std::shared_ptr;
using std::make_shared;

using namespace ppc::Parsing;

using ppc::Diagnostic::DiagnosticLevel;
using ppc::Diagnostic::DiagnosticMessage;

class SourceCode {
public:
    shared_ptr<Lexer> lexer;
    shared_ptr<Parser> parser;
    string fileName, code;
    std::vector<shared_ptr<Token>> tokens;
    shared_ptr<TranslationUnit> translationUnit;

    inline shared_ptr<DiagnosticMessage>
    createDiagMseeage(DiagnosticLevel level,
                      string msg,
                      shared_ptr<Token> token)
    {
        return make_shared<DiagnosticMessage>(level, this, msg, token);
    }

    SourceCode(string fileName, string code);
};

class SourceManager {
public:
    static SourceManager &getInstance(string path = string(),
                                      string code = string());
    static const char * const moduleSearchDir[];

    void addSource(string path, string code);
private:
    std::vector<std::shared_ptr<SourceCode>> sourceCodeList;
    std::unordered_map<string, std::shared_ptr<SourceCode>> codeMap;

    SourceManager(string path, string code);

    SourceManager &operator=(SourceManager &instance) { return instance; }

    ~SourceManager() { }
};

} // namespace ppc
#endif
