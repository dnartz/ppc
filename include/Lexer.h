#ifndef _LEXER_H
#define _LEXER_H

#include <vector>
#include <memory>
#include <string>

#include <DataList.h>

namespace ppc {

class SourceCode;

enum class tokType {
    NOTHING, ERROR_CHARACTER, NULL_VAL,

    ARROW, PERIOD, ELLIPSIS, COLON, COMMA, AT_SIGN,

    MINUS_MINUS, PLUS_PLUS,

    BITWISE_NOT,

    L_PAREN, R_PAREN, L_BRACKET, R_BRACKET, L_BRACE, R_BRACE,

    // Keywords
    TRUE, FALSE,

    STATIC_ASSERT,
    EXPORT, STATIC, THREAD_LOCAL, REGISTER_SPECIFIER, ALIGNAS,
    SETJMP, LONGJMP,
    FROM,

    VA_ARG, VA_START, VA_COPY, VA_END,
    OFFSETOF, SIZEOF, ALIGNOF,
    TYPEDEF, ENUM,
    STRUCT, UNION,

    CONST_QUALIFIER,VOLATILE_QUALIFIER, RESTRICT_QUALIFIER, ATOMIC_QUALIFIER,
    NORETURN_QUALIFIER,

    COMPLEX, IMAGINARY, INLINE,

#define V(type) type,
    FOREACH_BASIC_TYPE_SPECIFIER_TOKTYPE(V)
    FOREACH_BINARY_ARITHMETIC_OPERATOR_TOKTYPE(V)
    FOREACH_ASSIGNMENT_OPERATOR_TOKTYPE(V)
#undef V
    QUERY, TO,

    PRE_PROCESS_PRAGMA, PRE_PROCESS_INCLUDE,
    PRE_PROCESS_IF, PRE_PROCESS_ELIF, PRE_PROCESS_ELSE, PRE_PROCESS_IFDEF,
    PRE_PROCESS_IFNDEF, PRE_PROCESS_DEFINE, PRE_PROCESS_UNDEF,
    NO_AT_PRE_PROCESS_IFDEF, NO_AT_PRE_PROCESS_IFNDEF, PRE_PROCESS_ERROR,
    SWITCH, WHEN, CASE, DEFAULT, IF, UNLESS, UNLIKELY, THEN, ELSE, BY,
    DO, CONTINUE, BREAK, RETURN, GOTO, WHILE, FOR, LOOP,
    ON_SCOPE_EXIT,

    INT_CONST_DEC, INT_CONST_OCT, INT_CONST_HEX, INT_CONST_BIN,
    FLOAT_CONST, FLOAT_CONST_HEX,

    CHAR_CONST, WCHAR_CONST,
    STRING_CONST, WSTRING_CONST,
    HERE_STRING_CONST, HERE_WSTRING_CONST,

    __FUNC__, __PRETTY_FUNC__,

    IDENTIFIER
};

class Token {
public:
    std::shared_ptr<Token> next, prev;
    tokType type;
    std::string value, codeFragment;
    bool newLine = false, spaced = false;
    unsigned long column, lineno;

    Token(tokType type,
          std::string &value,
          std::string &code,
          unsigned long line,
          unsigned long col) :
    type(type),
    value(value),
    codeFragment(code),
    lineno(line),
    column(col) { }

    void remove() {
        if (this->prev) {
            this->prev->next = this->next;
        }

        if (this->next) {
            this->next->prev = this->prev;
        }

        this->prev = nullptr;
        this->next = nullptr;
    }
};

class Lexer {
    SourceCode *sourceCode;
    std::string &code;
    std::vector<std::shared_ptr<Token>> &tokens;

    void token(tokType type,
               const char *begin,
               unsigned int count,
               int lineno,
               int column);

    void error(std::shared_ptr<Token> tok);

public:
    Lexer(SourceCode *sourceCode);

    bool tokenize();

};

} // namespace ppc

#endif
