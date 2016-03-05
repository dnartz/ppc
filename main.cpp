#include <utility>
#include <string>
#include <fstream>
#include <iostream>

#include <SourceManager.h>
#include <AstGraphGenerator.h>

using ppc::SourceManager;

int main(int argc, char **argv) {
    std::ifstream fin(argv[1]);
    std::string code((std::istreambuf_iterator<char>(fin)),
                     std::istreambuf_iterator<char>());

    auto &sourceManager =
        SourceManager::getInstance(std::string(argv[1]), code);

    return 0;
}
