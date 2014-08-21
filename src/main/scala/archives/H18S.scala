package archives

import scala.collection.mutable.ArrayBuffer

/**
 * 90分でとけたよ！
 * 実は150分以内だったことが判明
 * @author apex
 */
object H18S {
  type Game = (Areas, Conn)
  type Areas = Seq[Seq[Int]]
  type Conn = Seq[Seq[Int]]

  def life(n: Int, conn: Conn) = 3 - conn(n).size

  def area(n: Int, areas: Areas) = areas.zipWithIndex.collect {
    case (points, area) if points.contains(n) => area
  }

  def removables(conn: Conn) = conn.zipWithIndex.collect {
    case (connection, point) if connection.size == 2 => point
  }

  def show(areas: Areas, conn: Conn) {
    println("areas")
    areas foreach println
    println("connections")
    conn foreach println
    println("area of points")
    1 to areas.flatten.distinct.size map (i => i -> area(i, areas)) foreach println
  }

  def getInitialPoints(areas: Areas, conn: Conn): Seq[Int] = {
    val cConn = conn.map(list => ArrayBuffer(list: _*)).toArray
    var rem = removables(cConn)
    var points = ArrayBuffer(areas.flatten.distinct: _*)
    while (rem.nonEmpty) {
      val rp = rem.head
      val cons = cConn(rp)
      val Seq(a, b) = cons.toSeq
      cConn(rp).clear()
      cConn(a) -= rp
      cConn(b) -= rp
      points -= rp
      rem = removables(cConn)
    }
    points
  }

  def getPossibleConnection(areas: Areas, conn: Conn): Seq[(Boolean, Int, Int)] = {
    import utils.algorithm.Graphs._
    //this is ok
    def connected(a: Int, b: Int): Boolean = a == b || dfsVertices(a)(conn).contains(b)
    def gpcFromArea(area: Seq[Int]) = {
      val combs = area.filter(life(_, conn) > 0).combinations(2).collect {
        case Seq(a, b) => (connected(a, b), a, b)
      }
      val selves = area.collect {
        case p if life(p, conn) >= 2 => (true, p, p)
      }
      combs ++ selves
    }
    areas.flatMap(gpcFromArea).groupBy {
      case (f, a, b) => (a :: b :: Nil).sorted
    }.values.map(_.head).toSeq
  }

  def showGame(g: (Areas, Conn)) {
    println("GAME=========================================")
    val (areas, connections) = g
    show(areas, connections)
    println("-------------")
    println("initial points:" + getInitialPoints(areas, connections).size)
    println("-------------")
    getPossibleConnection(areas, connections).sortBy {
      case (f, a, b) => (a, b)
    }.map {
      case (f, a, b) => (if (f) "#" else "") +(a, b)
    } foreach println
  }

  def main(args: Array[String]) {
    import archives.Patterns._
    showGame(game1_3)
    showGame(game1_2)
    showGame(game2)
  }
}

object Patterns {
  val game1_3 = (
    Seq(Nil, 1 :: 2 :: 4 :: 5 :: Nil, 1 :: 3 :: 5 :: Nil),
    Seq(
      Nil,
      5 :: 5 :: 4 :: Nil,
      4 :: Nil,
      Nil,
      1 :: 2 :: Nil,
      1 :: 1 :: Nil
    )
    )
  val game1_2 = (
    Seq(
      Nil,
      1 :: 2 :: 3 :: 4 :: Nil
    ),
    Seq(
      Nil,
      4 :: Nil,
      4 :: Nil,
      Nil,
      1 :: 2 :: Nil
    )
    )
  val game2 = (
    Seq(
      Nil,
      1 :: 3 :: 4 :: 5 :: Nil,
      2 :: 5 :: 1 :: Nil
    ),
    Seq(
      Nil,
      5 :: 5 :: Nil,
      Nil,
      Nil,
      Nil,
      1 :: 1 :: Nil
    )
    )
}
