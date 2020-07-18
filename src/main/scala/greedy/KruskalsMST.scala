package greedy

import utilities.{ Node, DirectedGraph, Edge }

// get the minimum weight edge - add to the graph if it does not create a cycle

object Kruskals {

    def buildMST[T](graph: DirectedGraph[T]): DirectedGraph[T] = {
        
        val mst = new DirectedGraph[T]()
        val edges = graph.getEdges()
        val edgesSorted = edges.sortBy(_.weight)
        var numNodes = graph.getNumNodes
        var mstEdges = 0

        edgesSorted.foreach(edge => {
            if(mstEdges+1 < numNodes) {
                mst.addEdge(edge)
                if(mst.isCycle) mst.removeEdge(edge)
                else mstEdges += 1
            }
        })

        return mst
    }
}

object KruskalsTest {

    def test(): DirectedGraph[Int] = {
        val graph = new DirectedGraph[Int]()
        graph.addConnection(0, 1, 10)
        graph.addConnection(0, 2, 6)
        graph.addConnection(0, 3, 5)
        graph.addConnection(1, 3, 15)
        graph.addConnection(2, 3, 4)
        return Kruskals.buildMST(graph)
    }
}