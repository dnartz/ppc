cmake_minimum_required(VERSION 3.3)
project(ppc)

include_directories("include")

add_custom_target(GenerateLexer
        COMMAND "re2c" -8bo LexerTokenizer.cpp Lexer_re2c.cpp
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -stdlib=libc++")

FILE(GLOB SOURCE_FILES include/*.h)

set(SOURCE_FILES
        ${SOURCE_FILES}
        main.cpp
        LexerTokenizer.cpp
        Lexer.cpp
        SourceManager.cpp
        Diagnostic.cpp
        Parser.cpp
        ParseDeclaration.cpp
        ParseExpression.cpp
        AstGraphGenerator.cpp
        AstAcceptVisitor.cpp
        Ast.cpp
        ParseExpression.cpp
        ParseStatement.cpp)
add_executable(ppc ${SOURCE_FILES})
add_dependencies(ppc GenerateLexer)