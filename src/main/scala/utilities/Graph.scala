package utilities

case class DirectedEdge[T](fromNode: T, toNode: T, weight: Double)

/* abtract Graph class */

object Graph {

    case class Edge[T](adjNodeID: T, weight: Double) { // Edge is used in internal representation
        def toDirectedEdge(fromNode: T) = DirectedEdge(fromNode, adjNodeID, weight) 
    }

    class Node[T](val id: T) { 
        val neighbors = new scala.collection.mutable.ListBuffer[Edge[T]]()
    }
}

abstract class Graph[T] {

    import Graph.{Node, Edge}
    import scala.collection.mutable.ListBuffer

    val nodes = scala.collection.mutable.Map[T, Node[T]]()

    def getNumNodes(): Int = nodes.size
    
    def contains(id: T):Boolean = nodes.contains(id)

    def addConnection(id1: T, id2: T, weight: Double): Unit
    def removeConnection(id1: T, id2: T): Unit

    def getNode(id: T):Node[T] = {
        assume(nodes.contains(id), "node missing")
        return nodes(id)
    }

    def isCycle(): Boolean // abstract - as could be optimized for directed graphs
}

/* adjancency list implementation - optimized with Map - optimzed for insert - directed graph */

final class DirectedGraph[T] extends Graph[T] {

    import scala.collection.mutable.{Map, ListBuffer}
    import Graph.{Node, Edge}
    
    override def addConnection(id1: T, id2: T, weight: Double = 1.0): Unit = {
        if(!nodes.contains(id1)) nodes += id1 -> new Node(id1)
        if(!nodes.contains(id2)) nodes += id2 -> new Node(id2)
        nodes(id1).neighbors += Edge(id2, weight)
    }

    override def removeConnection(id1: T, id2: T): Unit = {
        val edge = nodes(id1).neighbors.find(_.adjNodeID == id2)
        if(edge == None) throw new Exception("no connection found")
        nodes(id1).neighbors -= edge.get
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
            val neighborParent = find(neighbor.adjNodeID)
            if(nodeParent == neighborParent) cycle = true
            else parent(nodeParent) = Some(neighborParent) // union operation
        }

        return cycle
    }

    def getEdges(): ListBuffer[DirectedEdge[T]] = {
        val edges = ListBuffer[DirectedEdge[T]]()
        nodes.foreach(node => node._2.neighbors.foreach(edge => edges += edge.toDirectedEdge(node._1)))
        return edges
    }

    def addEdge(edge: DirectedEdge[T]): Unit = {
        addConnection(edge.fromNode, edge.toNode, edge.weight)
    }

    def removeEdge(edge: DirectedEdge[T]): Unit = {
        removeConnection(edge.fromNode, edge.toNode)
    }
}

final class UndirectedGraph[T] extends Graph[T] {

    import scala.collection.mutable.Map
    import Graph.{Node, Edge}

    override def addConnection(id1: T, id2: T, weight: Double = 1.0) {
        if(!nodes.contains(id1)) nodes += id1 -> new Node(id1)
        if(!nodes.contains(id2)) nodes += id2 -> new Node(id2)
        nodes(id1).neighbors += Edge(id2, weight)
        nodes(id2).neighbors += Edge(id1, weight)
    }

    override def removeConnection(id1: T, id2: T): Unit = {
        val id1Toid2Edge = nodes(id1).neighbors.find(_.adjNodeID == id2)
        val id2Toid1Edge = nodes(id2).neighbors.find(_.adjNodeID == id1)
        if(id1Toid2Edge == None) throw new Exception("no connection found")
        if(id2Toid1Edge == None) throw new Exception("no connection found")
        nodes(id1).neighbors -= id1Toid2Edge.get
        nodes(id2).neighbors -= id2Toid1Edge.get
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
            val neighborParent = find(neighbor.adjNodeID)
            if(nodeParent == neighborParent) cycle = true
            else {
                parent(nodeParent) = Some(neighborParent) // union operation
                seen += node._1
            }
        }

        return cycle
    }
}