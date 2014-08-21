package utils

import java.awt.BorderLayout
import javax.swing.JFrame

import processing.core.PApplet

import scala.reflect.ClassTag

class NoLoop extends PView{
  override def keyTyped(): Unit = {
    point(key,key)//this works! even under noLoop condition.
  }
}
trait PView extends PApplet{
  override def setup(): Unit = {
    size(800,800)
    smooth()
  }

  override def draw(): Unit = {
  }
}
object NoLoop{
  def main(args: Array[String]) {
    PUtil[NoLoop]()
  }
}

object PUtil{
  def apply[T:ClassTag](){
    PApplet.main(implicitly[ClassTag[T]].runtimeClass.getCanonicalName)
  }
  def show(applet:PApplet,w:Int = 600,h:Int = 600):JFrame = {
    val frame = new JFrame()
    frame.setLayout(new BorderLayout())
    frame.setSize(w,h)
    frame.add(applet,BorderLayout.CENTER)
    applet.init()
    applet.setPreferredSize(frame.getSize)
    frame.pack()
    frame.setVisible(true)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setLocation((applet.displayWidth-w)/2,(applet.displayHeight-h)/2)
    frame
  }
}
