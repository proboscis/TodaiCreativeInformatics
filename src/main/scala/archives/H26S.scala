import processing.core.PApplet
import utils.math.{Edge, V2, Triangle}
import scala.annotation.tailrec
import Math._
import scala.collection.immutable.NumericRange
object H26S{
  //make a triangle from an edge
  def dPoints(x: NumericRange[Double], y: NumericRange[Double]) = x.flatMap(a => y.map(b => V2(a, b)))
  type Edge = (V2,V2)
  def toParts(e: Edge): (Triangle, Seq[Edge]) = {
    val (a,b) = e
    val dir = b - a
    val third = dir / 3
    val begin = a + third
    val middle = begin + (third rot (-PI / 3d))
    val end = a + (third * 2d)
    (
      Triangle(begin, middle, end),
      Seq(a, begin, middle, end, b).sliding(2).map {
        case a :: b :: Nil => (a, b)
      }.toSeq
     )
  }
  @tailrec
  def koch(triangles: Seq[Triangle], edges: Seq[Edge], n: Int): (Seq[Triangle], Seq[Edge]) = n match {
    case 0 => (triangles, edges)
    case _ =>
      val (newTriangles,newEdges) = (edges map toParts).unzip
      koch(triangles ++ newTriangles, newEdges.flatten, n - 1)
  }
  def koch(t: Triangle, n: Int): (Seq[Triangle], Seq[Edge]) = koch(t :: Nil, t.edges, n)
  lazy val R0 = (d:Double) => dPoints(0d to 10d by d, 0d to 10d by d)
  lazy val R1 = R0 andThen (_.filter(p => pow(p.x-5,2) + pow(p.y-5,2) <= 25))
  @tailrec
  def kochArea(l:Double,area:Double,edges:Int,n:Int):Double = n match{
    case 0 => area
    case _ => kochArea(l/3,area + l * sqrt(3)*l/2 * edges,edges * 4,n-1)
  }
  def kochArea(l:Double,n:Int):Double = kochArea(l,l*sqrt(3)*l/2,3,n)
  def kochPoints = (d:Double,n:Int)=>{
    val (triangles, _) = koch(Triangle(V2(0, 0), V2(10, 0), V2(5, sqrt(3d) / 2d * 10d)), n)
    dPoints(0d to 10d by 0.1d, 0d to 10d by 0.1d).par.filter {
      p => triangles.exists(_.contains(p))
    }
  }
  def q1 = R0 andThen (_.size)
  def q2 = (d:Double) => (R1(d).size.toDouble/R0(d).size.toDouble)/4d
  def q3 = kochArea(10,2)
  def q4 = kochArea(10,_:Int)
  def q5 = kochPoints(_:Double,2)
  def q6 = kochPoints(_:Double,_:Int)
}

class View extends PApplet{
  import H26S._
  val (triangles, edges) = koch(Triangle(V2(0, 0), V2(10, 0), V2(5, sqrt(3d) / 2d * 10d)), 5)
  println(triangles.size)
  val points = dPoints(0d to 10d by 0.1d, 0d to 10d by 0.1d).par.filter {
    p => triangles.exists(_.contains(p))
  }
  println(points.size)

  val scaled = edges map {
    case (a,b) => (a*60,b*60f)
  }

  override def setup(): Unit = {
    //super.setup()
    size(600,600)
  }

  override def draw(): Unit = {
    //super.draw()
    for((V2(x1,y1),V2(x2,y2)) <- scaled){
      line(x1.toFloat,y1.toFloat,x2.toFloat,y2.toFloat)
    }
  }
}
object View{
  def main(args: Array[String]) {
    PApplet.main("View")
  }
}