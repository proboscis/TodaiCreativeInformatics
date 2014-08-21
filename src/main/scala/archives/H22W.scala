package archives

import utils.io.IOUtil

import scala.collection.mutable.ArrayBuffer
/**
 * @author glyph
 */
object H22W {

  import IOUtil._

  type V = Set[Int]
  type E = (Int, Int)

  trait Op {
    def a: Int
    def b: Int
  }

  case class AddVA(a: Int, b: Int) extends Op

  case class DelA(a: Int, b: Int) extends Op

  case class G() {
    val mat = Array.fill(10001, 10001)(false)

    def process(op: Op) {
      op match {
        case AddVA(a, b) => {
          mat(a)(b) = true
        }
        case DelA(a, b) => {
          mat(a)(b) = false
        }
      }
    }

    def numEdges = {
      mat.map(_.count(identity)).sum
    }
  }

  lazy val a = fileLines("data/h22w/a.txt").toArray
  lazy val b = fileLines("data/h22w/b.txt").map(parseLine).toArray

  def parseLine(line: String): Op = {
    if (line.startsWith("!")) {
      val Array(a, b) = line.substring(1).split("->").map(_.toInt)
      DelA(a, b)
    } else {
      val Array(a, b) = line.split("->").map(_.toInt)
      AddVA(a, b)
    }
  }

  def toArrow(line:String) = {
    val Array(a,b) = line.split("->").map(_.toInt)
    a->b
  }

  def fileToArrows(f: String) = fileLines(f).map(toArrow).toSeq

  def toVertices(arrows: Seq[(Int, Int)]) = arrows.flatMap {
    case (x, y) => x :: y :: Nil
  }.toSet

  //graph as matrix.
  def toMatrix(arrows: Seq[(Int, Int)]): Array[Array[Boolean]] = {
    val nV = 10001
    val matrix = Array.fill(nV, nV)(false)
    for ((a, b) <- arrows) {
      matrix(a)(b) = true
    }
    matrix
  }

  def q1() {
    val arrows = a.map(toArrow)
    val vertices = toVertices(arrows)
    val matrix = toMatrix(arrows)
    val a1 = vertices.size
    println(a1)
    val largeV = matrix.sortBy(_.count(identity)).last
    val a2 = largeV.count(identity)
    println(a2)
    def getTv = {
      val tv = arrows.toStream.scanLeft(Set[Int]()) {
        case (set, (a, b)) => set ++ Set(a, b)
      }.map(_.size).takeWhile(_ <= 1000).zipWithIndex.last._2
      println(tv)
      tv
    }
    println(getTv)
    def getTr = {
      val mat = Array.fill(10001, 10001)(false)
      var R = Set[Int](0)
      val it = arrows.iterator
      var continue = true
      var i = 0
      while (it.hasNext && continue) {
        val (a, b) = it.next
        mat(a)(b) = true
        if (R(a) || R(b)) {
          R = searchMatrix(0, mat).toSet
          if (R.size >= 1000) {
            continue = false
          }
        }
        i += 1
      }
      i
    }
    println(getTr)
    val join = arrows.zipWithIndex.collectFirst {
      case ((a, b), i) if a == 0 || b == 0 => i
    }
    println(join)
  }

  def q2() {
    val g = G()
    b.foreach(g.process)
    println("Ab:" + g.mat.map(_.count(identity)).sum)
    println("|Rb|:" + searchMatrix(0, g.mat).size)
    def getTr = {
      val results = new ArrayBuffer[Int]()
      val g = G()
      var R = Set[Int](0)
      val it = b.iterator
      var i = 0
      var less = true
      while (it.hasNext) {
        val op = it.next
        println(op)
        g.process(op)
        if (R(op.a) || R(op.b)) {
          R = searchMatrix(0, g.mat).toSet //ここをしっかり手続きしないといけないとはね//

          if (less && R.size >= 1000) {
            less = true
            results += i
          } else if (less && R.size < 1000) {
            less = false
          }
        }
        i += 1
      }
      results
    }
    println("Tr:" + getTr)
  }

  def main(args: Array[String]) {
    q2()

  }

  def searchMatrix(v:Int,mat:Array[Array[Boolean]]) ={
    import utils.algorithm.Graphs._
    val map = mat.map{
      conn => conn.iterator.zipWithIndex.collect{
        case (flag,i) if flag => i
      }
    }
    dfsVertices(v)(map)
  }
}
