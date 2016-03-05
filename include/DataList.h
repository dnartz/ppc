#ifndef _AST_LIST_H
#define _AST_LIST_H

#define FOREACH_ASSIGNMENT_OPERATOR_TOKTYPE(V) \
V(EQUALS)\
V(PLUS_EQUAL)\
V(MINUS_EQUAL)\
V(TIMES_EQUAL)\
V(MOD_EQUAL)\
V(DIV_EQUAL)\
V(R_SHIFT_EQUAL)\
V(L_SHIFT_EQUAL)\
V(XOR_EQUAL)\
V(OR_EQUAL)\
V(AND_EQUAL)\
V(QUESTION_EQUAL)\
V(DEDUCE_EQUAL)

#define FOREACH_BINARY_ARITHMETIC_OPERATOR_TOKTYPE(V) \
V(LOGIC_IS)\
V(LOGIC_ISNT)\
V(LOGIC_NOT)\
V(LOGIC_AND)\
V(LOGIC_OR)\
V(IN)\
V(GT)\
V(LT)\
V(GTE)\
V(LTE)\
V(MOD)\
V(PLUS)\
V(MINUS)\
V(TIMES)\
V(DIVIDE)\
V(R_SHIFT)\
V(L_SHIFT)\
V(BITWISE_XOR)\
V(BITWISE_OR)\
V(BITWISE_AND)

#define FOREACH_BASIC_TYPE_SPECIFIER_TOKTYPE(V) \
V(I8)\
V(I16)\
V(I32)\
V(I64)\
V(U8)\
V(U16)\
V(U32)\
V(U64)\
V(INT)\
V(L_INT)\
V(LL_INT)\
V(UINT)\
V(L_UINT)\
V(LL_UINT)\
V(S_INT)\
V(S_UINT)\
V(SIZE_T)\
V(USIZE_T)\
V(JMP_BUF)\
V(VA_LIST)\
V(PTRDIFF)\
V(DOUBLE)\
V(L_DOUBLE)\
V(FLOAT)\
V(CHAR)\
V(UCHAR)\
V(WCHAR)\
V(UWCHAR)\
V(CHAR_16)\
V(UCHAR_16)\
V(CHAR_32)\
V(UCHAR_32)\
V(VOID)

#define FOREACH_STORAGE_SPECIFIER_TOKTYPE(V) \
V(TYPEDEF)\
V(STATIC)\
V(REGISTER_SPECIFIER)\
V(THREAD_LOCAL)

#define FOREACH_TYPE_QUALIFIER_TOKTYPE(V) \
V(CONST_QUALIFIER)\
V(RESTRICT_QUALIFIER)\
V(VOLATILE_QUALIFIER)\
V(ATOMIC_QUALIFIER)

#define FOREACH_BASIC_TYPE_LITERAL_TOKTYPE(V) \
V(IDENTIFIER)\
V(INT_CONST_DEC)\
V(INT_CONST_HEX)\
V(INT_CONST_OCT)\
V(INT_CONST_BIN)\
V(FLOAT_CONST)\
V(FLOAT_CONST_HEX)\
V(CHAR_CONST)\
V(WCHAR_CONST)\
V(STRING_CONST)\
V(WSTRING_CONST)\
V(HERE_STRING_CONST)\
V(TRUE)\
V(FALSE)\
V(NULL_VAL)\
V(__FUNC__)\
V(__PRETTY_FUNC__)\
V(HERE_WSTRING_CONST)

#define FOREACH_EXPRESSION_NODE(V) \
V(Literal)\
V(CompoundLiteral)\
V(FunctionDefinition)\
V(BinaryExpression)\
V(BraceExpression)\
V(IfExpression)\
V(LongjmpExpression)\
V(VaArgExpression)\
V(OffsetOfExpression)\
V(SizeOfExpression)\
V(AlignOfExpression)\
V(ArrayAccess)\
V(StructOrUnionFieldAccess)\
V(ArgumentExpressionList)\
V(UnaryArithmeticExpression)\
V(UndeterminedCall)\
V(CastExpression)

#define FOREACH_TYPE_SPECIFIER_NODE(V) \
V(EnumSpecifier)\
V(BasicTypeSpecifier)\
V(ArrayTypeSpecifier)\
V(PointerTypeSpecifier)\
V(StructOrUnionSpecifier)\
V(FunctionTypeSpecifier)\
V(TypedefSpecifier)\

#define FOREACH_STMT_NODE(V) \
V(OnScopeExit)\
V(VarOrTypedefDecl)\
V(WhileStmt)\
V(DoWhileStmt)\
V(LoopStmt)\
V(ForFromStmt)\
V(ForInStmt)\
V(SwitchLabel)\
V(SwitchStmt)\
V(GotoStmt)\
V(GotoLabel)\
V(ReturnStmt)\
V(Annotation)\
V(ContinueOrBreak)

#define FOREACH_AST_NODE(V) \
V(Block)\
V(TranslationUnit)\
V(Identifier)\
V(Enumerator)\
V(StructOrUnionField)\
V(StructOrUnionFieldDecl)\
V(FunctionParamDecl)\
V(StaticAssertDecl)\
V(DeclInit)\
V(PragmaDecl)\
V(PreProcessCondition)\
FOREACH_STMT_NODE(V)\
FOREACH_TYPE_SPECIFIER_NODE(V)\
FOREACH_EXPRESSION_NODE(V)

#endif
