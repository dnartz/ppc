#ifndef _AST_VISITOR_H
#define _AST_VISITOR_H

#include <Ast.h>
#include <DataList.h>

namespace ppc {
namespace Parsing {

class AstVisitor {

#define V(nodeName) virtual void visit(nodeName &node) {};
public:
    FOREACH_AST_NODE(V)
#undef V
};

} // namespace Parsing
} // namespace ppc

#endif
