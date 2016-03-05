#include <utility>
#include <memory>
#include <string>
#include <fstream>

#include <Ast.h>
#include <AstVisitor.h>
#include <AstGraphGenerator.h>
#include <SourceManager.h>

namespace ppc {

using std::string;
using ppc::Diagnostic::DiagnosticMessage;

SourceCode::SourceCode(string fileName, string code) :
fileName(fileName),
code(code)
{
    this->lexer = make_shared<Lexer>(this);
    this->parser = make_shared<Parser>(this);

    this->lexer->tokenize();
    this->translationUnit = parser->parseTranslationUnit();

    Parsing::AstGraphGenerator gen;
    this->translationUnit->accept(gen);

    std::ofstream fout("/Users/LiuTao/Documents/ppc/test.gv");
    fout << gen.getGraphSource();
    fout.close();
    exit(1);
}

// TODO: Make it work with these alternative header search path:
// /usr/local/include
// /usr/target/include
// /usr/include
const char *const ppc::SourceManager::moduleSearchDir[] = {
        "/Users/LiuTao/Documents/ppc/stdlib"
};

SourceManager::SourceManager(string path, string code) {
    this->addSource(path, code);
}

void SourceManager::addSource(string path, string code) {
    if (this->codeMap[path]) {
        return;
    }

    auto sourceCode =
            make_shared<SourceCode>(path.substr(path.find_last_of('/') + 1),
                                    code);

    this->sourceCodeList.push_back(sourceCode);
    this->codeMap[path] = sourceCode;
}

SourceManager &SourceManager::getInstance(string path, string code) {
    static SourceManager *instance = nullptr;

    if (!path.empty() && !code.empty()) {
        instance = new SourceManager(path, code);
    }

    return *instance;
}

} // namespace ppc
