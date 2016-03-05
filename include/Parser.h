#ifndef _PARSER_H
#define _PARSER_H

#include <memory>
#include <string>
#include <iterator>
#include <vector>
#include <stack>

#include <Diagnostic.h>
#include <Lexer.h>
#include <Ast.h>

#define CONSUME(type) this->consume(begin, tokType::type)

#define MARK() auto __mark = begin
#define RELEASE() begin = __mark
#define RELEASE_AND_RETURN() RELEASE();return nullptr

#define PARSE_ERROR(msg) this->error(DiagnosticLevel::Error, (msg), \
    begin == this->tokens.end() ? this->tokens.back() : *begin)

#define FATAL_ERROR(msg, tok) this->error(DiagnosticLevel::Error, (msg), \
    (tok) ? (tok) : this->tokens.back(), true)

#define CUR_TOK (*begin)

namespace ppc {
namespace Parsing {

using ppc::Token;
using ppc::tokType;
using std::vector;
using std::shared_ptr;
using ppc::Diagnostic::DiagnosticMessage;
using ppc::Diagnostic::DiagnosticLevel;

typedef vector<shared_ptr<Token>>::iterator tokIt;

class Parser {
    SourceCode *sourceCode;
    vector<shared_ptr<Token>> &tokens;

    shared_ptr<Token> consume(tokIt &current, tokType const type);

    //
    // Declaration
    //
    shared_ptr<TypeSpecifier> parseTypeSpecifier(tokIt &begin);

    shared_ptr<TypedefSpecifier> parseTypedefSpecifier(tokIt &begin);
    shared_ptr<BasicTypeSpecifier> parseBasicTypeSpecifier(tokIt &begin);

    shared_ptr<TypeSpecifier> parseExplicitAggregateTypeSpecifier(tokIt &begin);
    shared_ptr<TypeSpecifier> parseImplicitAggregateTypeSpecifier(tokIt &begin);

    shared_ptr<TypeSpecifier> parseLeftExplicitTypeSpecifier(tokIt &begin);
    shared_ptr<TypeSpecifier> parseLeftImplicitTypeSpecifier(tokIt &begin);

    shared_ptr<RightTypeOperator>
    parseRightTypeOperator(tokIt &begin, shared_ptr<TypeSpecifier> left);

    shared_ptr<ArrayTypeSpecifier>
    parseArrayTypeOperator(tokIt &begin, shared_ptr <TypeSpecifier> left);

    shared_ptr<FunctionTypeSpecifier>
    parseFunctionTypeOperator(tokIt &begin, shared_ptr <TypeSpecifier> left);
    shared_ptr<FunctionParamDecl> parseFunctionParamDecl(tokIt &begin);

    shared_ptr<Token> inline parseTypeQualifier(tokIt &begin);
    bool parseTypeQualifierSequence(tokIt &begin,
                                            shared_ptr <Token> &leftmostQualifier,
                                            bool &isConst, bool &isRestrict,
                                            bool &isVolatile, bool &isAtomic);

    shared_ptr <VarOrTypedefDecl> parseVarDecl(tokIt &begin);
    bool parseVarInitList(tokIt &begin, shared_ptr <VarOrTypedefDecl> decl);

    shared_ptr<Token> parseStorageSpecifier(tokIt &begin);

    shared_ptr<PointerTypeSpecifier>
    parseExplicitPointerTypeSpecifier(tokIt &begin);
    shared_ptr<PointerTypeSpecifier>
    parseImplicitPointerTypeSpecifier(tokIt &begin);

    shared_ptr<EnumSpecifier> parseImplicitEnumSpecifier(tokIt &begin);
    shared_ptr<EnumSpecifier> parseExplicitEnumSpecifier(tokIt &begin);

    shared_ptr<Enumerator> parseEnumerator(tokIt &begin);
    bool parseEnumeratorList(tokIt &begin, shared_ptr<EnumSpecifier> specifier);

    shared_ptr<StructOrUnionSpecifier>
    parseImplicitStructOrUnionSpecifier(tokIt &begin);
    shared_ptr<StructOrUnionSpecifier>
    parseExplicitStructOrUnionSpecifier(tokIt &begin);

    bool
    parseStructOrUnionFieldDeclList(tokIt &begin,
                     shared_ptr<StructOrUnionSpecifier> structOrUnionSpecifier);
    bool
    parseStructOrUnionFieldList(tokIt &begin,
                                shared_ptr<StructOrUnionFieldDecl> fieldDecl);

    shared_ptr<StaticAssertDecl> parseStaticAssertDecl(tokIt &begin);

    shared_ptr<AstNode> parseExternalDeclaration(tokIt &begin);

    //
    // Block
    //
    std::vector<shared_ptr<Block>> blockContext;
    shared_ptr <Block> parseBlock(tokIt &begin, bool parseImport = false);

    //
    // Statment
    //
    shared_ptr <AstNode> parseStatement(tokIt &begin, bool parseImport = false);

    shared_ptr<OnScopeExit> parseOnScopeExit(tokIt &begin);
    shared_ptr<WhileStmt> parseWhileStmt(tokIt &begin);
    shared_ptr<DoWhileStmt> parseDoWhileStmt(tokIt &begin);
    shared_ptr<LoopStmt> parseLoopStmt(tokIt &begin);
    shared_ptr<ForStmt> parseForStmt(tokIt &begin);
    shared_ptr<ForInStmt> parseForInStmt(tokIt &begin);
    shared_ptr<ForFromStmt> parseForFromStmt(tokIt &begin);
    void parseForStmtCond(tokIt &begin,
                          shared_ptr<Expression> &condExpr,
                          shared_ptr<Expression> &stepExpr);

    shared_ptr<SwitchStmt> parseSwitchStmt(tokIt &begin);
    shared_ptr<SwitchLabel> parseSwitchLabel(tokIt &begin);

    shared_ptr<ReturnStmt> parseReturnStmt(tokIt &begin);
    shared_ptr<GotoLabel> parseGotoLabel(tokIt &begin);
    shared_ptr<GotoStmt> parseGotoStmt(tokIt &begin);
    shared_ptr<ContinueOrBreak> parseContinueOrBreak(tokIt &begin);
    shared_ptr<PragmaDecl> parsePragmaStmt(tokIt &begin);
    shared_ptr<Annotation> parseAnnotation(tokIt &begin);
    shared_ptr<AstNode> parseSingleLineStmt(tokIt &begin);

    //
    // Expression
    //
    inline shared_ptr<Expression> parseExpression(tokIt &begin) {
        return this->parseAssignmentExpression(begin);
    }
    shared_ptr<Expression> parsePrimaryExpression(tokIt &begin);
    shared_ptr<Expression> parseLiteral(tokIt &begin);

    //
    // Unary Expression
    //
    shared_ptr<Token> parseUnaryOptr(tokIt &begin);
    shared_ptr<Expression> parseUnaryExpression(tokIt &begin);
    shared_ptr<Expression> parseAssignmentExpression(tokIt &begin);

    shared_ptr <Expression>
    parsePostfixOptr(tokIt &begin, shared_ptr<Expression> prefix);
    shared_ptr<Expression> parsePostfixExpression(tokIt &begin);

    shared_ptr<Expression> parseCastExpression(tokIt &begin);

    shared_ptr<Expression> parseBinaryArithmeticExpression(tokIt &begin);

    //
    // CompoundLiteral
    //
    shared_ptr<CompoundLiteral::kvPair> parseExprInitializer(tokIt &begin);
    shared_ptr<CompoundLiteral::kvPair> parseExprKeyInitializer(tokIt &begin);
    shared_ptr<CompoundLiteral::kvPair>
    parseIdentifierKeyInitializer(tokIt &begin);
    shared_ptr <CompoundLiteral>
    parseInitializerList(tokIt &begin, shared_ptr <Token> left);
    shared_ptr<CompoundLiteral> parseCompoundLiteral(tokIt &begin);

    shared_ptr<Expression> parseIfExpression(tokIt &begin);

    shared_ptr<ArgumentExpressionList> parseArgumentExpressionList(tokIt &begin);
    shared_ptr<FunctionDefinition>
    parseFunctionDefinition(tokIt &begin, bool isAnonymous);
    inline void parseFunctionQualifier(tokIt &begin,
                                       shared_ptr<Token> &inlineTok,
                                       shared_ptr<Token> &noReturnTok)
    {
        inlineTok = nullptr;
        noReturnTok = nullptr;

        for (int i = 0; i < 2; i++) {
            if (auto tok = CONSUME(INLINE)) {
                if (inlineTok) {
                    FATAL_ERROR("Duplicated 'inline'.", tok);
                }
                inlineTok = tok;
            } else if (auto tok = CONSUME(NORETURN_QUALIFIER)) {
                if (noReturnTok) {
                    FATAL_ERROR("Duplicated 'noreturn'.", tok);
                }
                noReturnTok = tok;
            } else {
                break;
            }
        }
    }

    //
    // Preprocess directive
    //
    shared_ptr <PreProcessCondition>
    parsePreProcessCondition(tokIt &begin, bool allowNoAtSign = false);
    shared_ptr<PreProcessCondition> parsePreProcessElseGroup(tokIt &begin);
    shared_ptr<PreProcessCondition> parsePreProcessElifGroup(tokIt &begin);

    //
    // Diagnostic
    //
    void error(DiagnosticLevel lv,
               std::string msg,
               shared_ptr<Token> tok,
               bool fatal = false);

    void errorUnmatchedParentheses(std::shared_ptr<Token> left);
    void errorUnexpectedToken(std::shared_ptr<Token> token, bool fatal = false);
    void errorExpectToken(std::shared_ptr<Token> token,
                          std::string expect,
                          bool fatal = false);

public:
    shared_ptr<DiagnosticMessage> deepestErr;

    shared_ptr<TranslationUnit> parseTranslationUnit();

    Parser(SourceCode *sourceCode);
};

} // namespace parser
} // namespace ppc

#endif
