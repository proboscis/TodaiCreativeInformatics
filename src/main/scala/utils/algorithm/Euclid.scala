package utils.algorithm

/**
 * @author glyph
 */
object Euclid {
  /**
   * greatest common divisor
   * @param m m >= n
   * @param n
   * @tparam N
   * @return
   */
  def gcd[N:Integral](m:N,n:N):N = {
    val ev = implicitly[Integral[N]]
    if(n == ev.zero) m else gcd(n, ev.minus(m,ev.times(ev.quot(m,n),n)))
  }

  /**
   * greatest common divisor
   * @param m
   * @param n
   * @return
   */
  def gcdInt(m:Int,n:Int):Int = if(n == 0) m else gcdInt(n,m%n)
}
object EuclidTest{
  import Euclid._
  def main(args: Array[String]) {
    println(gcd(100,10))
    println(gcd(50,25))
  }
}
