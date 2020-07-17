package utilities

class Node[T](val id: T) { 
    val neighbors = new scala.collection.mutable.ListBuffer[T]() 
}

/* abtract Graph class */

abstract class Graph[T] {

    val nodes = scala.collection.mutable.Map[T, Node[T]]()
    def contains(id: T):Boolean = nodes.contains(id)
    def addConnection(id1: T, id2: T): Unit
    def removeConnection(id1: T, id2: T): Unit

    def getNode(id: T):Node[T] = {
        assume(nodes.contains(id), "node missing")
        return nodes(id)
    }

    def isCycle(): Boolean // abstract - as could be optimized for directed graphs
}

/* adjancency list implementation - optimized with Map - optimzed for insert - directed graph */

final class DirectedGraph[T] extends Graph[T] {

    import scala.collection.mutable.Map
    
    override def addConnection(id1: T, id2: T): Unit = {
        if(!nodes.contains(id1)) nodes += id1 -> new Node(id1)
        if(!nodes.contains(id2)) nodes += id2 -> new Node(id2)
        nodes(id1).neighbors += id2
    }

    override def removeConnection(id1: T, id2: T): Unit = {
        nodes(id1).neighbors -= id2
    }

    override def isCycle(): Boolean = {

        val parent: Map[T, Option[T]]  = Map(nodes.keys.toList.map(x => Tuple2(x, None)):_*)

        def find(id: T): T = {
            if(parent(id) == None) return id
            else find(parent(id).get)
        }

        var cycle = false
        for(node <- nodes; neighbor <- node._2.neighbors) if(!cycle) {
            val nodeParent = find(node._1)
            val neighborParent = find(neighbor)
            if(nodeParent == neighborParent) cycle = true
            else parent(nodeParent) = Some(neighborParent) // union operation
        }

        return cycle
    }
}

final class UndirectedGraph[T] extends Graph[T] {

    import scala.collection.mutable.Map

    override def addConnection(id1: T, id2: T) {
        if(!nodes.contains(id1)) nodes += id1 -> new Node(id1)
        if(!nodes.contains(id2)) nodes += id2 -> new Node(id2)
        nodes(id1).neighbors += id2
        nodes(id2).neighbors += id1
    }

    override def removeConnection(id1: T, id2: T): Unit = {
        nodes(id1).neighbors -= id2
        nodes(id2).neighbors -= id1
    }

    override def isCycle(): Boolean = {

        val parent: Map[T, Option[T]] = scala.collection.mutable.Map(nodes.keys.toList.map(x => Tuple2(x, None)):_*)
        val seen = new scala.collection.mutable.ListBuffer[T] // needed for undirected graphs

        def find(id: T): T = {
            if(parent(id) == None) return id
            else find(parent(id).get)
        }

        var cycle = false
        for(node <- nodes; neighbor <- node._2.neighbors) if(!cycle && !seen.contains(neighbor)) {
            val nodeParent = find(node._1)
            val neighborParent = find(neighbor)
            if(nodeParent == neighborParent) cycle = true
            else {
                parent(nodeParent) = Some(neighborParent) // union operation
                seen += node._1
            }
        }

        return cycle
    }
}