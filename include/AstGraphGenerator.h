#ifndef _ASTGRAPHGENERATOR_H
#define _ASTGRAPHGENERATOR_H

#include <vector>
#include <string>
#include <memory>

#include <DataList.h>
#include <AstVisitor.h>

namespace ppc {
namespace Parsing {

using std::string;

class AstGraphGenerator : public AstVisitor {
private:
    typedef unsigned long graphCount;

    class AstGraphNode {
    public:
        unsigned long id;
        graphCount nFields = 0;
        string nodeFields, nodeFieldsSuffix;

        void inline addField(string add);

#define V(nodeType) AstGraphNode(nodeType const &node):id(node.id)\
        {this->nodeFields = "<f0>"#nodeType;}
        FOREACH_AST_NODE(V)
#undef V
    };

    class AstGraphEdge {
    public:
        graphCount fromNode, fromField, toNode, toField;

        AstGraphEdge(graphCount fromNode, graphCount fromField,
                     graphCount toNode, graphCount toField) :
        fromNode(fromNode),
        fromField(fromField),
        toNode(toNode),
        toField(toField)
        { }
    };

    template <typename nodeType>
    inline std::shared_ptr<AstGraphNode> addNode(nodeType const &node);

    void inline addEdge(AstNode const &fromNode, graphCount fromField,
                        AstNode const &toNode, graphCount toField = 0);

    std::vector<std::shared_ptr<AstGraphNode>> nodes;
    std::vector<std::shared_ptr<AstGraphEdge>> edges;
public:
    string getGraphSource();

#define V(nodeType) virtual void visit(nodeType &node) override;
    FOREACH_AST_NODE(V)
#undef V
};

}
}
#endif
