package archives

import processing.core.PApplet
import utils.PUtil
import utils.algorithm.{Dijkstra, Graphs}

/**
 * @author apex
 */
object H19W {
  //16:00~18:20 done!
  import Graphs._

  def main(args: Array[String]) {
    //show(new View(BluePrint(bp2.lines)))
    //q4()
    q5()
    q6()
    q7()
  }
  def q7(){
    val bp3 = """"""
    val bp = BluePrint(bp3.lines)
    val dijkstra = Dijkstra((0,0))(bp.nexts.andThen(_.map(_->1d)))
    println(dijkstra.route(3,3))
  }
  def q6(){
    val bp = BluePrint(bp1.lines)
    val dijkstra = Dijkstra((0,0))(bp.nexts.andThen(_.map(_->1d)))
    println(dijkstra)
  }
  def q5(){
    val bp = BluePrint(bp2.lines)
    import bp._
    //x,y,horizontal?
    //return true if separated
    val visited = collection.mutable.Set[(Int,Int)]()
    val vertices = for(x <- 0 until N;y <- 0 until N) yield(x,y)
    val areas = collection.mutable.ArrayBuffer[Seq[(Int,Int)]]()
    vertices.foreach{
      p =>
        if(!visited(p)) {
          val area = dfsVertices(p)(bp.nexts).toList
          areas += area
          visited ++= area
        }
    }
    println(areas.map(_.size).sorted.reverse)
  }
  def q4(){
    show(new View(BluePrint(bp1.lines)))
  }
  def show(view:PApplet){
    PUtil.show(view)
  }
  val bp2 =
    """4
      |
      |(2,0)|
      |(1,1)|
      |(2,1)-
      |(2,1)|
      |(3,1)|
      |(1,2)|
      |(1,2)-
      |(3,2)|
      |(0,3)-
      |(1,3)-
      |(2,3)-
      |(3,3)-
      |(3,3)|
    """.stripMargin
  val bp1 =
    """4
      |
      |(0,1)-
      |(1,1)-
      |(2,1)-
      |(1,1)|
      |(2,1)|
      |(1,2)-
      |(3,2)-
      |(3,2)|
      |(1,3)-
      |(2,3)-
    """.stripMargin
}
case class BluePrint(N:Int,walls:Seq[Wall],doors:Seq[Door]){
  lazy val doorMap = doors.map{
    case Door(ax,ay,bx,by) => val warp = (ax,ay)->(bx,by)
      warp::warp.swap::Nil
  }.flatten.toMap
  lazy val blocked = walls.map{
    case Wall(x,y,true) =>val ver = ((x,y-1),(x,y))
      ver::ver.swap::Nil
    case Wall(x,y,false) => val hori = ((x-1,y),(x,y))
      hori::hori.swap::Nil
  }.flatten.toSet
  lazy val nexts = (p:(Int,Int)) => {
    val (x,y) = p
    val warp = if(doorMap.isDefinedAt(p)) doorMap(p)::Nil else Nil
    val neighbours = Seq((-1,0),(1,0),(0,-1),(0,1)).map{
      case (dx,dy) => (x + dx) -> (y + dy)
    }.filter{
      case (nx,ny) => !blocked((x,y)->(nx,ny))
    }
    neighbours ++ warp
  }
}
case class Door(ax:Int,ay:Int,bx:Int,by:Int)
case class Wall(x:Int,y:Int,hori:Boolean)
object BluePrint{
  def apply(src:Iterator[String]):BluePrint = {
    val N = src.next().toInt
    val WallRegex = """\((\d),(\d)\)([-|])""".r
    val DoorRegex = """\((\d),(\d)\)\*\((\d),(\d)\)""".r
    val objects = src.drop(1).collect{
      case WallRegex(x,y,d) =>
        Wall(x.toInt,y.toInt,if(d == "-") true else false)
      case DoorRegex(ax,ay,bx,by) => Door(ax.toInt,ay.toInt,bx.toInt,by.toInt)
    }.toStream
    val innerWalls = objects.collect{
      case wall@Wall(x,y,f)=> wall
    }
    val outerWalls = {
      (for(x <- 0 until N) yield Wall(x,0,true)::Wall(x,N,true)::Nil) ++
        (for(y <- 0 until N) yield Wall(0,y,false)::Wall(N,y,false)::Nil)
    }.flatten
    val doors = objects.collect{
      case d@Door(ax,ay,bx,by) => d
    }
    BluePrint(N,innerWalls ++ outerWalls,doors)
  }
}
class View(bp:BluePrint) extends PApplet{
  import bp._
  val D = 30
  override def setup(): Unit = {
    noLoop()
  }
  override def draw(): Unit = {
    translate((width - N*D)/2,(height-N*D)/2)

    drawDP()
    drawWalls()
  }
  def drawDP(){
    for(x <- 0 to N; y <- 0 to N){
      rect(x*D-5,y*D-5,10,10)
    }
  }
  def drawWalls(){
    for(Wall(x,y,hori) <- walls){
      val nx = if(hori) x+1 else x
      val ny = if(!hori) y+1 else y
      if(hori) {
        line(x * D, y * D,nx * D,ny * D)
      }else{
        line(x * D, y * D,nx * D,ny * D)
      }
    }
  }
}