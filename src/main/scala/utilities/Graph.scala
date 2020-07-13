package utilities

class Node(val id: Any) { 
    val neighbors = new scala.collection.mutable.MutableList[Node]() 
}

/* abtract Graph class */

abstract class Graph {
    val nodes = scala.collection.mutable.Map[Any, Node]()
    def contains(id: Any):Boolean = nodes.contains(id)
    def addConnection(id1: Any, id2: Any):Unit
    def getNode(id: Any):Node = {
        assume(nodes.contains(id), "node missing")
        return nodes(id)
    }
}

/* adjancency list implementation - optimized with Map - optimzed for insert - directed graph */

final class DirectedGraph extends Graph {
    override def addConnection(id1: Any, id2: Any):Unit = {
        if(!nodes.contains(id1)) nodes += id1 -> new Node(id1)
        if(!nodes.contains(id2)) nodes += id2 -> new Node(id2)
        nodes(id1).neighbors += nodes(id2) 
    }
}