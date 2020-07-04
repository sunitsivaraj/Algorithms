/* Minimum spanning tree implementation - Kruskals algorithm - Undirected Graph */

/* Adjecency List representation of List */
class Node(id:String) { 
    val neighbors = new scala.collection.mutable.MutableList[Tuple2[Node, Int]]()
}

/* additional space to store edges - to save time complexity to get edges */
abstract class Graph {
    val nodes = scala.collection.mutable.Map[String, Node]()
    def add_connection(id_1:String, id_2:String, weight:Int):Unit
    def contains(id:String):Boolean = nodes.contains(id)
    def getEdges():List[Tuple3[String, String, Int]]
    def hasCycle():Boolean
}


/* Undirected Graph Implementation */
final class UnDirectedGraph extends Graph {

    override def add_connection(id_1:String, id_2:String, weight:Int) = {
        if(!contains(id_1)) nodes(id_1) = new Node(id_1)
        if(!contains(id_2)) nodes(id_2) = new Node(id_2)
        nodes(id_1).neighbors += Tuple2(nodes(id_2), weight)
        nodes(id_2).neighbors += Tuple2(nodes(id_1), weight)
    }

    /* get all edges - undirected edges from the graph */
    override def getEdges():List[Tuple3[String, String, Int]] = {
        val edges = new scala.collection.mutable.MutableList[Tuple3[String, String, Int]]
        for(source_node <- nodes.keySet) {
            for(destination_node <- nodes(source_node).neighbors) {
                if(source_node.id > destination_node._1.id) 
                    edges += Tuple3(source_node.id, destination_node._1.id, destination_node._2)
            }
        }
        return edges
    }
}

/* returns the minimum spanning tree */
def kruskals(graph:Graph): Graph = {
    
    
    val edges_sorted = graph.getEdges.sortBy(edge => edge._3)
    for (edge <- edges_sorted) {

    }
}