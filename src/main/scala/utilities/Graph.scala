package utilities

class Node[T](val id: T) { 
    val neighbors = new scala.collection.mutable.MutableList[Node[T]]() 
}

/* abtract Graph class */

abstract class Graph[T] {
    val nodes = scala.collection.mutable.Map[T, Node[T]]()
    def contains(id: T):Boolean = nodes.contains(id)
    def addConnection(id1: T, id2: T):Unit
    def getNode(id: T):Node[T] = {
        assume(nodes.contains(id), "node missing")
        return nodes(id)
    }
}

/* adjancency list implementation - optimized with Map - optimzed for insert - directed graph */

final class DirectedGraph[T] extends Graph[T] {
    override def addConnection(id1: T, id2: T):Unit = {
        if(!nodes.contains(id1)) nodes += id1 -> new Node(id1)
        if(!nodes.contains(id2)) nodes += id2 -> new Node(id2)
        nodes(id1).neighbors += nodes(id2) 
    }
}

final class UndirectedGraph[T] extends Graph[T] {
    override def addConnection(id_1:T, id_2:T) {
        if(!nodes.contains(id_1)) nodes += id_1 -> new Node(id_1)
        if(!nodes.contains(id_2)) nodes += id_2 -> new Node(id_2)
        nodes(id_1).neighbors += nodes(id_2)
        nodes(id_2).neighbors += nodes(id_1)
    }
}