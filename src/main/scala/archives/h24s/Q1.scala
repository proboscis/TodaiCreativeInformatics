package archives.h24s

import java.awt.event.KeyEvent

import processing.core.PApplet
import utils.PUtil
import utils.syntax.IdOps

import scala.concurrent.Future

/**
 * @author apex
 */
object Q1 {
  import IdOps._
  import scala.concurrent.ExecutionContext.Implicits.global
  def main(args: Array[String]) {
    val board = new Board
    val items = board::Nil
    val view = new View(items,30f)
    val frame = PUtil.show(view)
  }
  trait Drawable{
    def draw(p:PApplet,scale:Float):Unit
  }
  class Board extends Drawable{
    val ROW = 9
    val COL = 15
    override def draw(p: PApplet, scale: Float): Unit = {
      import p.{line,strokeWeight,stroke}
      strokeWeight(1/scale)
      stroke(122)
      for(y <- 0 until COL) {
        line(0,y,ROW-1,y)
      }
      for(x <- 0 until ROW){
        line(x,0,x,COL-1)
      }
    }
  }
  class View(items:Seq[Drawable],scale:Float) extends PApplet{
    override def setup(): Unit = {
      noLoop()
    }
    override def draw(): Unit = {
      clear()
      translate((width-scale*9)/2,(height-scale*15)/2)
      scale(scale)
      items foreach (_.draw(this,scale))
    }
    override def keyTyped(e: KeyEvent): Unit = {
      redraw()
    }
  }
}
