package utils.algorithm

import archives.H22W.G
import utils.TimeUtil
import utils.io.IOUtil

import scala.collection.{immutable => i, mutable => m}

/**
 * @author glyph
 */
object Graphs {
  /**
   * @param start node
   * @param neighbors function that provides connected nodes and their scores
   * @return map of costs without parents.
   */
  def dijkstra(start: Int, neighbors: Int => Seq[(Int, Int)]): i.Map[Int, Int] = {
    val d = m.HashMap.empty[Int, Int].withDefaultValue(Int.MaxValue) //node, from , dist
    val heap = m.PriorityQueue(start -> 0)(Ordering.by(_._2))
    while (heap.nonEmpty) {
      val (currentNode, currentCost) = heap.dequeue()
      for (edge@(next, cost) <- neighbors(currentNode)) {
        val newCost = cost + currentCost
        if (d(next) > newCost) {
          d(next) = newCost
          heap.enqueue(next -> newCost)
        }
      }
    }
    d.toMap
  }

  /**
   * @param goal destination in integer
   * @param costs map that contains parent information. this function assume -1 in parent as root.
   * @return routes and cost
   */
  def dijkstraToRoute(goal: Int, costs: i.Map[Int, (Int, Int)]): (i.List[Int], Int) = {
    var list = goal :: Nil
    while (costs(list.head)._1 != -1) {
      val (from, cost) = costs(list.head)
      list ::= from
    }
    list -> costs(goal)._2
  }

  /**
   * O((E+V)logV)
   * @param node starting node
   * @param nexts function that provides children of given node
   * @return Map[destination,(parent,cost)]
   */
  def dijkstraWithRouteCost(node: Int, nexts: Int => Seq[(Int, Int)]): i.Map[Int, (Int, Int)] = {
    implicit val order = new Ordering[(Int, Int)] {
      override def compare(x: (Int, Int), y: (Int, Int)): Int = x._2 - y._2
    }
    val d = m.HashMap.empty[Int, (Int, Int)].withDefaultValue((-1) -> Int.MaxValue) //node, from , dist
    val heap = m.PriorityQueue(node -> 0)(Ordering.by(_._2))
    while (heap.nonEmpty) {
      val (currentNode, currentCost) = heap.dequeue()
      for (edge@(next, cost) <- nexts(currentNode)) {
        val (_, prevCost) = d(next)
        val newCost = cost + currentCost
        if (prevCost > newCost) {
          d(next) = currentNode -> newCost
          heap.enqueue(next -> newCost)
        }
      }
    }
    d.toMap.withDefaultValue((-1) -> Int.MaxValue)
  }

  def genericDijkstraWithRouteCost[G](node: G, nexts: G => Seq[(G, Int)]): i.Map[G, (G, Int)] = {
    implicit val order = new Ordering[(G, Int)] {
      override def compare(x: (G, Int), y: (G, Int)): Int = x._2 - y._2
    }
    val d = m.HashMap.empty[G, (G, Int)].withDefaultValue(null.asInstanceOf[G] -> Int.MaxValue) //node, from , dist
    d(node) = null.asInstanceOf[G]->0
    val heap = m.PriorityQueue(node -> 0)(Ordering.by(_._2))
    while (heap.nonEmpty) {
      val (currentNode, currentCost) = heap.dequeue()
      for (edge@(next, cost) <- nexts(currentNode)) {
        val (_, prevCost) = d(next)
        val newCost = cost + currentCost
        if (prevCost > newCost) {
          d(next) = currentNode -> newCost
          heap.enqueue(next -> newCost)
        }
      }
    }
    d.toMap.withDefaultValue(null.asInstanceOf[G] -> Int.MaxValue)
  }

  def genericDijkstraToRoute[G](dst: G, map: i.Map[G, (G, Int)]): (i.List[G], Int) = {
    var list = dst :: Nil
    while (map(list.head)._1 != null) {
      val (from, cost) = map(list.head)
      list ::= from
    }
    list -> map(dst)._2
  }

  /**
   * this version that uses bfs iterator is a bit faster for some reason.
   * @param start
   * @param next
   * @tparam T
   * @return
   */
  def distanceMap[T](start: T, next: T => Seq[T]): Map[T, Int] = {
    (Map(start -> 0).withDefaultValue(Int.MaxValue) /: bfsEdges(start)(next)) {
      case (map, (current, n)) => if (map(n) > map(current) + 1) {
        map + (n -> (map(current) + 1))
      } else map
    }
  }

  def dfsLoopEdges[T](start: T)(next: T => Seq[T])(op: (T, T) => Any) {
    val stack = collection.mutable.Stack(start)
    val visited = collection.mutable.Set(start)
    while (stack.nonEmpty) {
      val current = stack.pop()
      for (n <- next(current) if !visited(n)) {
        op(current, n)
        stack.push(n)
        visited += n
      }
    }
  }

  def dfsLoopNodes[T](start: T)(next: T => Seq[T])(op: T => Any) {
    val stack = collection.mutable.Stack(start)
    val visited = collection.mutable.Set(start)
    while (stack.nonEmpty) {
      val current = stack.pop()
      op(current)
      for (n <- next(current) if !visited(n)) {
        stack.push(n)
        visited += n
      }
    }
  }

  def bfsLoopEdges[T](start: T)(next: T => Seq[T])(op: (T, T) => Any) {
    val stack = collection.mutable.Queue(start)
    val visited = collection.mutable.Set(start)
    while (stack.nonEmpty) {
      val current = stack.dequeue()
      for (n <- next(current) if !visited(n)) {
        op(current, n)
        stack.enqueue(n)
        visited += n
      }
    }
  }

  def bfsLoopNodes[T](start: T)(next: T => Seq[T])(op: T => Any) {
    val stack = collection.mutable.Queue(start)
    val visited = collection.mutable.Set(start)
    while (stack.nonEmpty) {
      val current = stack.dequeue()
      op(current)
      for (n <- next(current) if !visited(n)) {
        stack.enqueue(n)
        visited += n
      }
    }
  }

  /**
   * 最小木の構築: O(E log E)
   * @param start starting node
   * @param vertices all vertices as Set
   * @param neighbours a function that returns seq of edges:(given,next,cost)
   * @tparam T type of nodes
   * @tparam N type of costs
   * @return edges that construct MST
   */
  def prim[T, N: Ordering](start: T, vertices: Set[T])(neighbours: T => Seq[(T, T, N)]): Seq[(T, T, N)] = {
    val visited = collection.mutable.Set(start)
    val edges = m.ArrayBuffer[(T, T, N)]()
    val queue = m.PriorityQueue[(T, T, N)](neighbours(start): _*)(Ordering.by(_._3))
    while (visited != vertices) {
      while (visited(queue.head._2)) queue.dequeue() //there are deprecated edges in the queue!
      val edge@(_, nv, _) = queue.dequeue()
      edges += edge
      visited += nv
      queue ++= neighbours(nv)
    }
    edges
  }

  /**
   * 最小木の構築: O(E log E), sorting takes O(ELogE), collecting takes O(ELogE
   * @param edges all possible edges with costs
   * @tparam T type of vertices
   * @tparam N type of costs
   * @return edges that construct MST
   **/
  def kruskal[T, N: Ordering](edges: Seq[(T, T, N)]): Seq[(T, T, N)] = {
    val uf = new UnionFind[T]
    edges.sortBy(_._3).collect {
      case e@(a, b, cost) if !uf(a, b) => {
        uf.unify(a, b)
        e
      }
    }
  }

  /**
   * iterates through vertices of given graph
   * @param col
   * @param start starting vertex
   * @param neighbors adjacent list
   * @tparam T type of vertex
   * @return
   */
  def vertexIterator[T](col: GraphCol[T])(start: T)(neighbors: T => TraversableOnce[T]) = new Iterator[T] {
    col.add(start)
    val visited = collection.mutable.Set(start)

    override def hasNext: Boolean = col.nonEmpty

    override def next(): T = {
      val c = col.get()
      for (n <- neighbors(c) if !visited(n)) {
        col.add(n)
        visited += n
      }
      c
    }
  }

  /**
   * iterates through edges of given graph
   * @param col
   * @param start starting vertex
   * @param neighbors
   * @tparam T
   * @return
   */
  def edgeIterator[T](col: GraphCol[T])(start: T)(neighbors: T => TraversableOnce[T]): Iterator[(T, T)] = new Iterator[(T, T)] {
    col.add(start)
    val visited = collection.mutable.Set(start)
    val buffer = m.Queue[(T, T)]()

    override def hasNext: Boolean = {
      //try to fill if the buffer is empty
      while (buffer.isEmpty && col.nonEmpty) {
        val c = col.get()
        for (n <- neighbors(c) if !visited(n)) {
          col add n
          visited += n
          buffer += c -> n
        }
      }
      buffer.nonEmpty
    }

    override def next(): (T, T) = buffer.dequeue()
  }

  /**
   * iterates through vertices of given graph
   * @param col stack for dfs, queue for bfs
   * @param start starting vertex
   * @param neighbors adjacent list
   * @param visitAs convert T to F which is passed to visited sets
   * @tparam T type of vertex
   * @tparam F type used for evaluating whether vertex is visited or not
   * @return iterator of vertices
   */
  def vertexIterator2[T, F](col: GraphCol[T])(start: T)(neighbors: T => TraversableOnce[T])(visitAs: T => F): Iterator[T] = new Iterator[T] {
    col.add(start)
    val visited = collection.mutable.Set(visitAs(start))

    override def hasNext: Boolean = col.nonEmpty

    override def next(): T = {
      val c = col.get()
      for (n <- neighbors(c) if !visited(visitAs(n))) {
        col.add(n)
        visited += visitAs(n)
      }
      c
    }
  }

  /**
   * simple cycle checker
   * @param start
   * @param neighbors
   * @tparam T
   * @return true if the graph is cyclic, false if acyclic
   */
  def checkCycle[T](start: T)(neighbors: T => TraversableOnce[T]): Boolean = {
    val col = new GraphStack[T]()
    col.add(start)
    val visited = collection.mutable.Set(start)
    var cycle = false
    while (col.nonEmpty && !cycle) {
      val c = col.get()
      visited += c
      val nexts = neighbors(c).toSeq
      cycle = nexts.exists(visited)
      if (!cycle) nexts.filterNot(visited).foreach(col.add)
    }
    cycle
  }

  /**
   * all graphs in a given graph are tree => given graph is a forest
   * @param graph
   * @tparam T
   * @return true if the graph is empty or contains no cyclic graph, else false
   */
  def acyclic[T](graph: Map[T, Seq[T]]): Boolean = graph.isEmpty || {
    val outputs = graph.values.flatten.toSeq
    val vertices = (graph.keys ++ graph.values.flatten).toSeq.distinct
    val roots = vertices.filterNot(outputs.contains)
    roots.forall{
      v =>
      val col = new GraphStack[T]()
      col.add(v)
      val visited = collection.mutable.Set(v)
      var acycle = true
      while (col.nonEmpty && acycle) {
        val v = col.get()
        visited += v
        val nexts = graph(v)
        acycle = !nexts.exists(visited)
        if (acycle) nexts.filterNot(visited).foreach(col.add)
      }
      acycle
    }
  }
  /**
   * check whether the given graph contains a cyclic graph or not
   * @param graph
   * @tparam T
   * @return true if the graph contains cyclic graph, else false
   **/
  def cyclic[T](graph:Map[T,Seq[T]]):Boolean = graph.nonEmpty && !acyclic(graph)

  /**
   * iterates through edges of given graph
   * @param col stack for dfs, queue for bfs
   * @param start starting vertex
   * @param neighbors adjacent list
   * @param visitAs convert T to F which is passed to visited sets
   * @tparam T type of vertex
   * @tparam F type used for evaluating whether vertex is visited or not
   * @return iterator of edges
   */
  def edgeIterator2[T, F](col: GraphCol[T])(start: T)(neighbors: T => TraversableOnce[T])(visitAs: T => F): Iterator[(T, T)] = new Iterator[(T, T)] {
    col.add(start)
    val visited = collection.mutable.Set(visitAs(start))
    val buffer = m.Queue[(T, T)]()

    override def hasNext: Boolean = {
      //try to fill if the buffer is empty
      while (buffer.isEmpty && col.nonEmpty) {
        val c = col.get()
        for (n <- neighbors(c) if !visited(visitAs(n))) {
          col add n
          visited += visitAs(n)
          buffer += c -> n
        }
      }
      buffer.nonEmpty
    }


    override def next(): (T, T) = buffer.dequeue()
  }

  def dfsEdges[T](start: T)(neighbors: T => TraversableOnce[T]) =
    edgeIterator(new GraphStack[T]())(start)(neighbors)

  def dfsVertices[T](start: T)(neighbors: T => TraversableOnce[T]) =
    vertexIterator(new GraphStack[T]())(start)(neighbors)

  def bfsEdges[T](start: T)(neighbors: T => TraversableOnce[T]) =
    edgeIterator(new GraphQueue[T]())(start)(neighbors)

  def bfsVertices[T](start: T)(neighbors: T => TraversableOnce[T]) =
    vertexIterator(new GraphQueue[T]())(start)(neighbors)

  def dfsEdges2[T, F](start: T)(visitAs: T => F)(neighbors: T => TraversableOnce[T]) =
    edgeIterator2(new GraphStack[T]())(start)(neighbors)(visitAs)

  def dfsVertices2[T, F](start: T)(visitAs: T => F)(neighbors: T => TraversableOnce[T]) =
    vertexIterator2(new GraphStack[T]())(start)(neighbors)(visitAs)

  def bfsEdges2[T, F](start: T)(visitAs: T => F)(neighbors: T => TraversableOnce[T]): Iterator[(T, T)] =
    edgeIterator2(new GraphQueue[T]())(start)(neighbors)(visitAs)

  def bfsVertices2[T, F](start: T)(visitAs: T => F)(neighbors: T => TraversableOnce[T]) =
    vertexIterator2(new GraphQueue[T]())(start)(neighbors)(visitAs)

  /**
   * is this working correctly?
   * sorts the acyclic graph topologically
   * does not work if the given graph was not acyclic!!!!
   * make sure to check it with method before using this method.
   * @param graph
   * @tparam T
   * @return
   */
  def topologicalSort2[T](graph: Map[T, Seq[T]]): Seq[T] = {
    val L = m.ArrayBuffer[T]()
    val vertices = (graph.keys ++ graph.values.flatten).toSeq.distinct
    val visited = m.Set[T]()
    //帰ってくるときに呼び出されるdfsが必要か...
    def visit(n: T) {
      if (!visited(n)) {
        visited += n
        graph(n).foreach(visit)
        L += n
      }
    }
    vertices.foreach(visit)
    L.reverse
  }

  /**
   * inefficient
   * @param graph
   * @tparam T
   * @return
   */
  def topologicalSort[T](graph: Map[T, Seq[T]]): Seq[T] = {
    val L = m.ArrayBuffer[T]()
    val visited = m.Set[T]()
    val vertices = (graph.keys ++ graph.values.flatten).toSeq.distinct
    vertices.foreach {
      v => dfsVerticesInv(v)(graph).filterNot(visited).foreach{
        n => visited += n
          L += n
      }
    }
    L.reverse
  }

  def dfsVerticesInv[T](start: T)(g: T => TraversableOnce[T]): Iterator[T] = new Iterator[T] {
    val stack = m.Stack(start)
    val called = m.Set[T]()
    val seen = m.Set[T]()
    val buf = m.Queue[T]()

    override def hasNext: Boolean = {
      while (buf.isEmpty && stack.nonEmpty) {
        val v = stack.pop()
        val children = g(v).toSeq
        if (children.isEmpty || children.forall(c => called(c) || seen(c))) {
          buf enqueue v
          called += v
        } else {
          stack.push(v)
          seen += v
          children.filterNot(seen).foreach {
            child =>
              seen += child
              stack.push(child)
          }
        }
      }
      buf.nonEmpty
    }

    override def next(): T = buf.dequeue()
  }

  /*
  def dfsVerticesBack[T](g:Map[T,Seq[T]]) ={
    val stack = m.Stack(g.keys.head)
    val called = m.Set[T]()
    while(stack.nonEmpty){
      val v = stack.pop()
      val children = g(v)
      if(children.isEmpty || children.forall(called)){
        //callback!
        called += v
      }else{
        stack.push(v)
        children.foreach(stack.push)
  import utils.algorithm.Graphs._
  */
}

object GraphsTest {

  import Graphs._

  lazy val graph = IOUtil.fileLines("data/h20s/edges.txt")
    .map(_.split(" "))
    .map { case Array(a, b) => a.toInt -> b.toInt}
    .scanLeft(Map[Int,Seq[Int]]().withDefaultValue(Nil)){
      case (map,(u,v)) =>
        map.updated(u,map(u):+v).withDefaultValue(Nil)
  }.takeWhile(acyclic).toStream.last

  def validateTopologicalSort[T](sorted: Seq[T])(graph: Map[T, Seq[T]]): Boolean = !graph.keys.exists {
    u => val predecessor = sorted.takeWhile(_ != u)
      graph(u).exists {
        v => predecessor.contains(v)
      }
  }

  def main(args: Array[String]) {
    val head = graph.keys.head
    val dfs = dfsVertices(head)(graph).toStream
    val dfsInv = dfsVerticesInv(head)(graph).toStream
    println("dfs\t\t:" + dfs.mkString(","))
    println("dfsinv\t:" + dfsInv.mkString(","))
    println(s"dfs: ${dfs.size} inv:${dfsInv.size}")
    println("validate dfs,dfsInv:" + (dfs.toSet == dfsInv.toSet))
    println("cyclic|acyclic => " + cyclic(graph) + "|" + acyclic(graph))
    import TimeUtil.{printTimeWithMessage => pt}
    val sort1 = pt("sort1")(topologicalSort(graph).toSeq)
    val sort2 = pt("sort2")(topologicalSort2(graph).toSeq)
    println("sort1:"+sort1)
    println("sort2:"+sort2)
    println("topological sort equality:", sort1 == sort2)
    println("validate topological sort:")
    val validator = validateTopologicalSort(_:Seq[Int])(graph)
    sort1::sort2::Nil map validator foreach println
  }
}

class Dijkstra[T](src:T)(conns:T=>TraversableOnce[(T,Double)]){
  val costs = {
    implicit val order = new Ordering[(T, Double)] {
      override def compare(x: (T, Double), y: (T, Double)): Int = (x._2 - y._2).toInt
    }
    val d = m.HashMap.empty[T, (T, Double)].withDefaultValue(null.asInstanceOf[T] -> Double.MaxValue) //node, from , dist
    d(src) = null.asInstanceOf[T] -> 0d
    val heap = m.PriorityQueue(src -> 0d)(Ordering.by(_._2))
    while (heap.nonEmpty) {
      val (currentNode, currentCost) = heap.dequeue()
      for (edge@(next, cost) <- conns(currentNode)) {
        val (_, prevCost) = d(next)
        val newCost = cost + currentCost
        if (prevCost > newCost) {
          d(next) = currentNode -> newCost
          heap.enqueue(next -> newCost)
        }
      }
    }
    d.toMap.withDefaultValue(null.asInstanceOf[T] -> Double.MaxValue)
  }
  def cost(dst:T) = costs(dst)._2
  def route(dst:T) = {
    val route = Iterator.iterate(dst::Nil){
      list =>
      val (from, cost) = costs(list.head)
      from :: list
    }.takeWhile(list => list.head != null).toStream.last
    route -> costs(dst)._2
  }

  override def toString: String = costs.map{
    case (dst,(parent,cost)) => """dst: %8s parent: %8s cost: %8s""".format(dst,parent,cost)
  }.mkString("\n")
}
object Dijkstra{
  def apply[T](src:T)(conns:T=>TraversableOnce[(T,Double)]) = new Dijkstra(src)(conns)
}

trait GraphCol[T] {
  def add(v: T): Unit

  def get(): T

  def nonEmpty: Boolean
}

class GraphStack[T](values: T*) extends GraphCol[T] {
  val stack = m.Stack[T](values: _*)

  override def add(v: T): Unit = stack.push(v)

  override def get(): T = stack.pop()

  override def nonEmpty: Boolean = stack.nonEmpty
}

class GraphQueue[T](values: T*) extends GraphCol[T] {
  val queue = m.Queue[T](values: _*)

  override def add(v: T): Unit = queue.enqueue(v)

  override def get(): T = queue.dequeue()

  override def nonEmpty: Boolean = queue.nonEmpty
}
