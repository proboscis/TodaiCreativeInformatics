package utils

/**
 * @author kento
 */
object TimeUtil {
  def nanoTime[T](f : =>T):(Long,T) ={
    val s = System.nanoTime()
    val r = f
    val e = System.nanoTime()
    (e-s,r)
  }
  def milliTime[T](f: =>T):(Long,T) ={
    nanoTime(f) match{
      case(a,b) => (a/1000/1000,b)
    }
  }
  def printTime[T](f: =>T):T = {
    val (t,r) = milliTime(f)
    println(t + " ms.")
    r
  }
  def printTimeWithMessage[T](msg:String)(f: =>T):T={
    print(msg+" => ")
    printTime(f)
  }
}
