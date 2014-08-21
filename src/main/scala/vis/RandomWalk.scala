package vis

import processing.core.PApplet

import scala.util.Random

/**
 * @author apex
 */
class RandomWalk extends PApplet{

  var lx, ly = 400
  override def setup(): Unit = {
    size(800,800)
  }

  override def draw(): Unit = {
    val nx = lx + Random.nextInt(3) - 1
    val ny = ly + Random.nextInt(3) - 1
    line(lx,ly,nx,ny)
    lx = nx
    ly = ny
  }
}

object RandomWalkView{
  def main(args: Array[String]) {
    PApplet.main("vis.RandomWalk")
  }
}
