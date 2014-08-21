package archives.h24s

import java.awt.event.KeyEvent

import processing.core.PApplet
import utils.PUtil
import utils.syntax.IdOps

import scala.reflect.ClassTag

/**
 * @author apex
 */
object Q3 {
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
        f => f.asInstanceOf[T=>_].apply(e)
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
  case class BulletHitBall(bullet:Bullet,ball:Ball)
  class PointUpdater(board:Board,em:EventManager) extends Updatable{
    private val points = m.ArrayBuffer[Point]()
    val queue = m.ArrayBuffer[Any]()
    def += (p:Point) = queue += Tuple2("add",p)
    def -= (p:Point) = queue += Tuple2("rem",p)
    override def update(): Unit = {
      import Math._
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
          if(p.y >= board.COL || p.y < 0){
            em << PointOut(p)
          }
      }
      val balls = points.collect{
        case c:Ball => c
      }
      val bullets = points.collect{
        case b:Bullet => b
      }
      for(ball <- balls){
        for(bullet <- bullets){
          if(bullet.x == ball.x && bullet.y == ball.y){
            em << BulletHitBall(bullet,ball)
          }
        }
      }
      queue.foreach{
        case ("add",p:Point) => points += p
        case ("rem",p:Point) => points -= p
      }
      queue.clear()
    }
  }
  class Bullet extends Drawable with Point{
    val R = 5
    override def draw(p: PApplet, scale: Float): Unit = {
      val r = R/scale
      p.fill(p.color(255))
      p.ellipse(x,y,r,r)
      p.fill(p.color(0,255,0))
      p.textSize(r.toInt+1)
      p.text((x,y,vx,vy)+"",x,y)
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
    var score = 0
    val em = new EventManager
    val objects = m.ArrayBuffer[Any](em)
    val board = new Board
    val collision = new PointUpdater(board,em)
    objects ++= collision :: board ::Nil
    em += ((e:BulletHitBall) => {
      queue += Tuple2("rem",e.ball)
      queue += Tuple2("rem",e.bullet)
      collision -= e.ball
      collision -= e.bullet
      score += 1
    })
    val queue = m.ArrayBuffer[Any]()
    em += ((e:PointOut)=>{
      queue += Tuple2("rem",e.p)
      collision -= e.p
    })
    def shoot(){
      val ball = new Ball
      ball.x = 4;ball.y = 0
      ball.vx = 1;ball.vy = 1
      collision += ball
      queue += Tuple2("add",ball)
    }
    def shootBullet(){
      val b= new Bullet
      b.x = 4;b.y = board.COL-1
      b.vy = 0;b.vy = -1
      collision += b
      queue += Tuple2("add",b)
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
      objects.foreach {
        case o: Drawable => o.draw(p, scale)
        case _ =>
      }
      p.fill(255,0,0)
      p.textSize(1)
      p.text("SCORE"+score+"",-1,-1)
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
      game.draw(this,scale)
    }
    override def keyTyped(e: KeyEvent): Unit = {
      if(game.objects.find(_.isInstanceOf[Ball]).isEmpty) {
        game.shoot()
      }
      if(key == 'i'){
        game.shootBullet()
      }
      game.update()
      redraw()
    }
  }
}
