package utils.syntax

import archives.RP.S

/**
 * @author glyph
 */
object IdOps {
  implicit class IdOpsImpl[T](val self:T) extends AnyVal{
    def |> [R](f:T=>R):R = f(self)
    def <| (f:T=>Any):T ={
      f(self)
      self
    }
  }
  implicit class IdFuncOps[-P,+R](val f :P=>R) extends AnyVal{
    def >>> [S](f2:R=>S):P=>S = f andThen f2
    def <<< [S](f2:S=>P):S=>R = f compose f2
  }
  implicit class IntOps(val self:Int) extends AnyVal{
    def times(proc: =>Any){
      for(i <- 1 to self){
        proc
      }
    }
  }
  /*
  implicit class Tuple2Ops[A,B](val self:(A,B)) extends AnyVal{
    def a = self._1
    def b = self._2
    def x = self._1
    def y = self._2
  }
  implicit class Tuple3Ops[A,B,C](val self:(A,B,C)) extends AnyVal{
    def a = self._1
    def b = self._2
    def c = self._3
    def x = self._1
    def y = self._2
    def z = self._3
  }
  */

  def main(args: Array[String]) {
    val c = 3 <| (_ + 1) <| (_ + 1)
    val p = (Seq(1),Seq(2))
    println(c)
    p.zipped.foreach((a,b)=>println(a,b))
  }
}
