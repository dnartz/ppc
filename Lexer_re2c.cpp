#include <Lexer.h>
#include <iostream>

#define TOKEN(type)                                                       \
this->token(tokType::type, tokStart, yycursor - tokStart, lineno, column);\
lastType = tokType::type;                                                 \
continue

namespace ppc {

bool Lexer::tokenize() {
    const char *yymarker,
            *yycursor = this->code.c_str(),
            *yyctxmarker = yycursor,
            *tokStart = yycursor;
    int column = 1, lineno = 1;
    enum tokType lastType = tokType::NOTHING;
    for (; ;) {
        if (lastType != tokType::NOTHING) {
            column += yycursor - tokStart;
        }

        tokStart = yycursor;

        if (lastType == tokType::STRING_CONST ||
            lastType == tokType::WSTRING_CONST ||
            lastType == tokType::HERE_STRING_CONST ||
            lastType == tokType::HERE_WSTRING_CONST) {
            for (auto c : this->tokens.back()->value) {
                if (c == '\n') {
                    lineno++;
                    column = 1;
                }
            }
        }
        /*!re2c
            re2c:yyfill:enable = 0;
            re2c:define:YYCTYPE = "unsigned char";
            re2c:define:YYCURSOR = "yycursor";
            re2c:define:YYMARKER = "yymarker";
            re2c:define:YYCTXMARKER = "yyctxmarker";

            "#"[^\n]* {continue;}

            "\x00" {return true;}

            " " {
                if (!this->tokens.empty()) {
                    this->tokens.back()->spaced = true;
                }
                lastType = tokType::NOTHING;
                column++;
                continue;
            }

            "\n"+ {
                if (!this->tokens.empty()) {
                    this->tokens.back()->newLine = true;
                }
                lastType = tokType::NOTHING;
                lineno += yycursor - tokStart;
                column = 1;
                continue;
            }

            "\r"+ {
                lastType = tokType::NOTHING;
                continue;
            }

            "true" {TOKEN(TRUE);}
            "false" {TOKEN(FALSE);}

            "is" {TOKEN(LOGIC_IS);}
            "isnt" {TOKEN(LOGIC_ISNT);}
            "not" {TOKEN(LOGIC_NOT);}
            "and" {TOKEN(LOGIC_AND);}
            "or" {TOKEN(LOGIC_OR);}
            "in" {TOKEN(IN);}

            ">" {TOKEN(GT);}
            "<" {TOKEN(LT);}
            ">=" {TOKEN(GTE);}
            "<=" {TOKEN(LTE);}

            "%" {TOKEN(MOD);}
            "+" {TOKEN(PLUS);}
            "-" {TOKEN(MINUS);}
            "*" {TOKEN(TIMES);}
            "/" {TOKEN(DIVIDE);}
            ">>" {TOKEN(R_SHIFT);}
            "<<" {TOKEN(L_SHIFT);}
            "^" {TOKEN(BITWISE_XOR);}
            "|" {TOKEN(BITWISE_OR);}
            "&" {TOKEN(BITWISE_AND);}

            "--" {TOKEN(MINUS_MINUS);}
            "++" {TOKEN(PLUS_PLUS);}

            "=" {TOKEN(EQUALS);}
            "+=" {TOKEN(PLUS_EQUAL);}
            "-=" {TOKEN(MINUS_EQUAL);}
            "*=" {TOKEN(TIMES_EQUAL);}
            "%=" {TOKEN(MOD_EQUAL);}
            "/=" {TOKEN(DIV_EQUAL);}
            ">>=" {TOKEN(R_SHIFT_EQUAL);}
            "<<=" {TOKEN(L_SHIFT_EQUAL);}
            "^=" {TOKEN(XOR_EQUAL);}
            "|=" {TOKEN(OR_EQUAL);}
            "&=" {TOKEN(AND_EQUAL);}
            "?=" {TOKEN(QUESTION_EQUAL);}
            ":=" {TOKEN(DEDUCE_EQUAL);}

            "->" {TOKEN(ARROW);}
            "." {TOKEN(PERIOD);}
            ":" {TOKEN(COLON);}
            "," {TOKEN(COMMA);}
            "?" {TOKEN(QUERY);}
            "..." {TOKEN(ELLIPSIS);}

            "~" {TOKEN(BITWISE_NOT);}
            "(" {TOKEN(L_PAREN);}
            ")" {TOKEN(R_PAREN);}
            "[" {TOKEN(L_BRACKET);}
            "]" {TOKEN(R_BRACKET);}
            "{" {TOKEN(L_BRACE);}
            "}" {TOKEN(R_BRACE);}

            "@" {TOKEN(AT_SIGN);}

            "void" {TOKEN(VOID);}

            "complex" {TOKEN(COMPLEX);}
            "imaginary" {TOKEN(IMAGINARY);}
            "static_assert" {TOKEN(STATIC_ASSERT);}

            "int" {TOKEN(INT);}
            "uint" {TOKEN(UINT);}
            "s_int" {TOKEN(S_INT);}
            "s_uint" {TOKEN(S_UINT);}
            "l_int" {TOKEN(L_INT);}
            "l_uint" {TOKEN(L_UINT);}
            "ll_int" {TOKEN(LL_INT);}
            "ll_uint" {TOKEN(LL_UINT);}

            "size_t" {TOKEN(SIZE_T);}
            "u_size_t" {TOKEN(USIZE_T);}
            "jmp_buf" {TOKEN(JMP_BUF);}
            "va_list" {TOKEN(VA_LIST);}
            "ptrdiff" {TOKEN(PTRDIFF);}

            "i8" {TOKEN(I8);}
            "i16" {TOKEN(I16);}
            "i32" {TOKEN(I32);}
            "i64" {TOKEN(I64);}

            "u8" {TOKEN(U8);}
            "u16" {TOKEN(U16);}
            "u32" {TOKEN(U32);}
            "u64" {TOKEN(U64);}

            "float" {TOKEN(FLOAT);}
            "double" {TOKEN(DOUBLE);}
            "l_double" {TOKEN(L_DOUBLE);}

            "char" {TOKEN(CHAR);}
            "uchar" {TOKEN(UCHAR);}
            "char_16" {TOKEN(CHAR);}
            "uchar_16" {TOKEN(UCHAR);}
            "char_32" {TOKEN(CHAR);}
            "uchar_32" {TOKEN(UCHAR);}
            "wchar" {TOKEN(WCHAR);}
            "uwchar" {TOKEN(UWCHAR);}

            "typedef" {TOKEN(TYPEDEF);}
            "struct" {TOKEN(STRUCT);}
            "union" {TOKEN(UNION);}
            "enum" {TOKEN(ENUM);}

            "sizeof" {TOKEN(SIZEOF);}
            "offsetof" {TOKEN(OFFSETOF);}
            "alignof" {TOKEN(ALIGNOF);}

            "from" {TOKEN(FROM);}

            "export" {TOKEN(EXPORT);}
            "static" {TOKEN(STATIC);}
            "inline" {TOKEN(INLINE);}
            "register" {TOKEN(REGISTER_SPECIFIER);}
            "thread_local" {TOKEN(THREAD_LOCAL);}
            "alignas" {TOKEN(ALIGNAS);}

            "const" {TOKEN(CONST_QUALIFIER);}
            "atomic" {TOKEN(ATOMIC_QUALIFIER);}
            "restrict" {TOKEN(RESTRICT_QUALIFIER);}
            "volatile" {TOKEN(VOLATILE_QUALIFIER);}
            "noreturn" {TOKEN(NORETURN_QUALIFIER);}

            "return" {TOKEN(RETURN);}

            "@if" {TOKEN(PRE_PROCESS_IF);}
            "@elif" {TOKEN(PRE_PROCESS_ELIF);}
            "@else" {TOKEN(PRE_PROCESS_ELSE);}
            "@ifdef" {TOKEN(PRE_PROCESS_IFDEF);}
            "@ifndef" {TOKEN(PRE_PROCESS_IFNDEF);}
            "@define" {TOKEN(PRE_PROCESS_DEFINE);}
            "@undef" {TOKEN(PRE_PROCESS_UNDEF);}
            "@error" {TOKEN(PRE_PROCESS_ERROR);}
            "@pragma" {TOKEN(PRE_PROCESS_PRAGMA);}
            "@include" {TOKEN(PRE_PROCESS_INCLUDE);}

            "ifdef" {TOKEN(NO_AT_PRE_PROCESS_IFDEF);}
            "ifndef" {TOKEN(NO_AT_PRE_PROCESS_IFNDEF);}

            "if" {TOKEN(IF);}
            "unless" {TOKEN(UNLESS);}
            "unlikely" {TOKEN(UNLIKELY);}
            "else" {TOKEN(ELSE);}
            "then" {TOKEN(THEN);}

            "goto" {TOKEN(GOTO);}
            "continue" {TOKEN(CONTINUE);}
            "break" {TOKEN(BREAK);}

            "setjmp" {TOKEN(SETJMP);}
            "longjmp" {TOKEN(LONGJMP);}

            "va_arg" {TOKEN(VA_ARG);}
            "va_start" {TOKEN(VA_START);}
            "va_copy" {TOKEN(VA_COPY);}
            "va_end" {TOKEN(VA_END);}

            "loop" {TOKEN(LOOP);}
            "for" {TOKEN(FOR);}
            "while" {TOKEN(WHILE);}
            "to" {TOKEN(TO);}
            "do" {TOKEN(DO);}
            "switch" {TOKEN(SWITCH);}
            "when" {TOKEN(WHEN);}
            "case" {TOKEN(CASE);}
            "by" {TOKEN(BY);}
            "default" {TOKEN(DEFAULT);}
            "on_scope_exit" {TOKEN(ON_SCOPE_EXIT);}

            "NULL" {TOKEN(NULL_VAL);}
            "__func__" {TOKEN(__FUNC__);}
            "__pretty_func__" {TOKEN(__PRETTY_FUNC__);}

            [a-zA-Z_][0-9a-zA-Z_]* {TOKEN(IDENTIFIER);}

            HEX_PREFIX = "0"[xX];
            HEX_DIGITS = [0-9a-fA-F]+;
            BIN_PREFIX = "0"[bB];
            BIN_DIGITS = [01]+;

            INTEGER_SUFFIX_OPT = (([uU]"ll")|([uU]"LL")|("ll"[uU]?)|("LL"[uU]?)|([uU][lL])|([lL][uU]?)|[uU])?;

            ("0"INTEGER_SUFFIX_OPT)|([1-9][0-9]*INTEGER_SUFFIX_OPT) {
                TOKEN(INT_CONST_DEC);
            }

            "0"[0-7]*INTEGER_SUFFIX_OPT {
                TOKEN(INT_CONST_OCT);
            }

            HEX_PREFIX HEX_DIGITS INTEGER_SUFFIX_OPT {
                TOKEN(INT_CONST_HEX);
            }

            BIN_PREFIX BIN_DIGITS INTEGER_SUFFIX_OPT {
                TOKEN(INT_CONST_BIN);
            }

            EXPONENT_PART = [eE][-+]?[0-9]+;
            FRACTIONAL_CONSTANT = ([0-9]*"."[0-9]+)|([0-9]+".");
            ((FRACTIONAL_CONSTANT EXPONENT_PART?)|([0-9]+EXPONENT_PART))[FfLl]? {
                TOKEN(FLOAT_CONST);
            }

            BINARY_EXPONENT_PART = [pP][+-]?[0-9]+;
            HEX_FRACTIONAL_CONSTANT = (HEX_DIGITS?"."HEX_DIGITS)|(HEX_DIGITS".");
            HEX_PREFIX(HEX_DIGITS|HEX_FRACTIONAL_CONSTANT)BINARY_EXPONENT_PART[FfLl]? {
                TOKEN(FLOAT_CONST_HEX);
            }

            SIMPLE_ESCAPE = [a-zA-Z._~!=&\^\-\\?'"];
            DECIMAL_ESCAPE = [0-9]+;
            HEX_ESCAPE = "x"[0-9a-fA-F]+;

            ESCAPE_SEQUENCE = "\\" (SIMPLE_ESCAPE|DECIMAL_ESCAPE|HEX_ESCAPE);
            CCONST_CHAR = [^\n\\']|ESCAPE_SEQUENCE;

            "'"CCONST_CHAR"'" {
                TOKEN(CHAR_CONST);
            }

            "L'"CCONST_CHAR"'" {
                TOKEN(WCHAR_CONST);
            }

           STRING_CHAR = [^"\\\n]|ESCAPE_SEQUENCE;
           HERE_STRING_CHAR = '\n'|STRING_CHAR;

           '"""'HERE_STRING_CHAR*'"""'/[^"] {
               TOKEN(HERE_STRING_CONST);
           }

           'L"""'HERE_STRING_CHAR*'"""'/[^"] {
               TOKEN(HERE_WSTRING_CONST);
           }

           '"'STRING_CHAR*'"'/[^"] {
               TOKEN(STRING_CONST);
           }

           'L"'STRING_CHAR*'"'/[^"] {
               TOKEN(WSTRING_CONST);
           }

           [^] {
               this->token(tokType::ERROR_CHARACTER,
                           tokStart,
                           yycursor - tokStart,
                           lineno,
                           column);
               this->error(this->tokens.back());
               return false;
           }
        */
    }
}

} // namespace ppc
