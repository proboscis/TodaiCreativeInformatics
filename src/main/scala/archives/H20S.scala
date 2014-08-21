package archives
import utils.TimeUtil._
import utils.algorithm.Graphs._
import utils.io.IOUtil
import utils.io.IOUtil._
import scala.collection.{immutable => i, mutable => m}
/**
 * q3までしかできなかった(90分)
 * 実は150分だったので行ける！
 * 一行づつ読み込み、グラフを構築していく場合・・
 * ノード数がそれほど多くなければ、scanLeftでimmutableな作り方をしたほうが良いだろう。
 * @author glyph
 */
object H20S {
  type M = Array[Array[Int]]
  type L = Array[Seq[Int]]
  type Node = Int
  type Edges = Seq[(Int,Int)]
  val N = 100
  lazy val edges = fileLines("data/h20s/edges.txt").toSeq.map(parseLine _ andThen toPair)
  def toPair(a:Array[Int]):(Int,Int) = a(0)->a(1)
  def parseLine(line: String) = line.split(" ").map(_.toInt)
  lazy val graphs = edges.scanLeft(Array.fill(N+1)(Seq.empty[Int])){
    case(ary,(a,b)) => ary.updated(a,ary(a) :+ b).updated(b,ary(b) :+ a)
  }

  def graphEdges(n: Int) = edges.take(n)
  def isFullyConnected(graph: L): Boolean = subTree(1, graph).size == N
  def subTree(start: Int, lists: L): Seq[Int] = dfsVertices(start)(lists).toSeq
  def allSubTree(lists: L): Seq[Seq[Int]] = {
    val results = m.ArrayBuffer[Seq[Int]]()
    val visited = m.Set[Int]()
    for (i <- 1 to N) {
      if (!visited(i)) {
        val tree = subTree(i, lists)
        visited ++= tree
        results += tree
      }
    }
    results
  }
  lazy val G2Lists = graphs(181)
  lazy val G2Edges = graphEdges(181)
  lazy val G3Lists = graphs(202)
  def kc2(k: Int) = (k * (k - 1)) / 2.0
  def clusterK(v: Int, g: L, edges: Edges) = {
    val neighbours = subTree(v, g).toSet - v
    edges.count {
      case (a, b) => neighbours(a) && neighbours(b)
    } / kc2(neighbours.size)
  }
  def allClusterK(g: L, edges: Edges) = (1 to N).map(clusterK(_, g, edges))
  def clusterMean(n:Int) = (1 to N).map(clusterK(_, graphs(n), graphEdges(n))).sum / n.toDouble
  // N*(N+E)
  def longest(g:Int=>Seq[Int]) = (1 to N).flatMap(distanceMap(_,g).values).max //NP HARD
  val questions = Map(
    "q2_1" -> (() => {
      allSubTree(G2Lists).map(_.size).sorted foreach println
    }),
    "q2_2" -> (() => {
      (1 to 10).map(clusterK(_, G2Lists, G2Edges)) foreach println
    }),
    "q2_3" -> (() => {
      println(clusterMean(181))
    }),
    "q2_4" -> (() => {
      IOUtil.fileLines("data/h20s/edges.txt").foreach {
        lines =>
          val graph = Array.fill(N + 1)(Seq.empty[Int])
          val numEdges = graphs.takeWhile(!isFullyConnected(_)).size
          println(numEdges)
          println(clusterMean(numEdges))
      }
    }),
    "q2_5" -> (() => {
      println(clusterMean(302))
    }),
    "q3" -> (() => {
      val g3List = graphs(202)
      val g4List = graphs(302)
      val g3Costs = distanceMap(27,g3List)
      val g4Costs = distanceMap(27,g4List)
      println(g3Costs(63),g4Costs(63))
      val g3AllCosts = (1 to N).map(distanceMap(_,g3List))
      val g4AllCosts = (1 to N).map(distanceMap(_,g4List))
      val g3Mean = (for(a<-0 until N ; b <- 1 to N) yield g3AllCosts(a)(b)).sum / 10000d
      val g4Mean = (for(a<-0 until N ; b <- 1 to N) yield g4AllCosts(a)(b)).sum / 10000d
      println(g3Mean,g4Mean)
    }),
    "exam/q4" -> (() => {
      var prev = longest(G3Lists)
      graphs.zipWithIndex.foreach {
        case (g, i) =>
          val l = longest(g)
          if (prev > l && i >= 202) {
            println("prev: %d now: %d at line %d".format(prev, l, i))
            prev = l
          }
      }
    })
  )
  def main(args: Array[String]) {
    questions.toSeq.sortBy(_._1).foreach{
      case(k,v)=>
        println("----------------")
        println(k)
        printTime(v())
        println("----------------")
    }
  }
}

