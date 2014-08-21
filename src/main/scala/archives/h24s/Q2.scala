package archives.h24s

import java.awt.event.KeyEvent

import archives.h24s.Q2.Updatable
import processing.core.PApplet
import utils.PUtil
import utils.syntax.IdOps

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/**
 * @author apex
 */
object Q2 {
  import IdOps._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.collection.{mutable => m}
  type ~>[A,B] = PartialFunction[A,B]
  def main(args: Array[String]) {
    val game = new Game
    val view = new View(game,30f)
    val frame = PUtil.show(view)
  }
  trait Drawable{
    def draw(p:PApplet,scale:Float):Unit
  }
  trait Updatable{
    def update():Unit
  }
  trait Point{
    var x:Int = 0
    var y:Int = 0
    var vx:Int = 0
    var vy:Int = 0
  }
  class EventManager extends Updatable{
    val listeners = m.Map[ClassTag[_],m.ArrayBuffer[Any]]()
    val queue = m.ArrayBuffer[Any]()
    def <<[T:ClassTag] (e:T) {
      val tag = implicitly[ClassTag[T]]
      listeners.getOrElseUpdate(tag, m.ArrayBuffer.empty[Any]).foreach{
        f => val fp = f.asInstanceOf[T~>_]
          if(fp.isDefinedAt(e))fp(e)
      }
    }
    def +=[T:ClassTag](listener:T=>Any){
      val tag = implicitly[ClassTag[T]]
      queue += Tuple3("add",tag,listener)
    }
    def -=[T:ClassTag](listener:T=>Any){
      val tag = implicitly[ClassTag[T]]
      queue += Tuple3("rem",tag,listener)
    }

    def update(){
      queue.foreach{
        case ("add",tag:ClassTag[_],listener) => listeners.getOrElseUpdate(tag,m.ArrayBuffer.empty) += listener
        case ("rem",tag:ClassTag[_],listener) => listeners.getOrElseUpdate(tag,m.ArrayBuffer.empty) -= listener
      }
      queue.clear()
    }
  }
  case class PointOut(p:Point)
  class PointUpdater(board:Board,em:EventManager) extends Updatable{
    private val points = m.ArrayBuffer[Point]()
    val queue = m.ArrayBuffer[Any]()
    def += (p:Point) = queue += Tuple2("add",p)
    def -= (p:Point) = queue += Tuple2("rem",p)
    override def update(): Unit = {
      import Math._
      //update points and keep them inside of the stage
      def mm(tgt:Int,mini:Int,maxi:Int) = min(max(mini,tgt),maxi)
      points.foreach{
        p =>
          p.x += p.vx
          p.y += p.vy
          if(p.x < 0){
            p.x *= -1
            p.vx *= -1
          } else if(p.x > board.ROW -1) {
            p.x = (board.ROW-1) - (p.x - (board.ROW-1)) //mm(p.x,0,board.ROW)
            p.vx *= -1
          }
          if(p.y >= board.COL){
            em << PointOut(p)
          }
      }
      queue.foreach{
        case ("add",p:Point) => points += p
        case ("rem",p:Point) => points -= p
      }
      queue.clear()
    }
  }
  class Ball extends Drawable with Point{
    val R = 10
    override def draw(p: PApplet, scale: Float): Unit = {
      val r = R/scale
      p.fill(p.color(255))
      p.ellipse(x,y,r,r)
      p.fill(p.color(0,255,0))
      p.textSize(r.toInt+1)
      p.text((x,y,vx,vy)+"",x,y)
    }
  }
  class Game extends Updatable with Drawable{
    val em = new EventManager
    val objects = m.ArrayBuffer[Any](em)
    val board = new Board
    val collision = new PointUpdater(board,em)
    objects ++= collision :: board ::Nil
    val queue = m.ArrayBuffer[Any]()
    def shoot(){
      val ball = new Ball
      ball.x = 4;ball.y = 0
      ball.vx = 1;ball.vy = 1
      collision += ball
      queue += Tuple2("add",ball)
      var listener:PointOut~>Any = null;
      listener = {
        case PointOut(p:Ball) if p == ball =>
          queue += Tuple2("rem",ball)
          em -= listener
      }
      em += listener
    }
    override def update(): Unit = {
      objects.foreach{
        case o:Updatable => o.update()
        case _ =>
      }
      queue.foreach{
        case ("rem",obj) => objects -= obj
        case ("add",obj) => objects += obj
      }
      queue.clear()
    }
    override def draw(p: PApplet, scale: Float): Unit = {
      objects.foreach{
        case o:Drawable => o.draw(p,scale)
        case _ =>
      }
    }
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
  class View(game:Game,scale:Float) extends PApplet{
    override def setup(): Unit = {
      noLoop()
    }
    override def draw(): Unit = {
      clear()
      translate((width-scale*9)/2,(height-scale*15)/2)
      scale(scale)
      game.objects.foreach {
        case o:Drawable => o.draw(this,scale)
        case _ =>
      }
    }
    override def keyTyped(e: KeyEvent): Unit = {
      if(game.objects.find(_.isInstanceOf[Ball]).isEmpty) {
        game.shoot()
      }
      game.update()
      redraw()
    }
  }
}
