package utils.math

/**
 * @author glyph
 */


case class M22(
                a: Double, b: Double,
                c: Double, d: Double
                ) {
  def +(m: M22) = ???

  def -(m: M22) = ???

  def *(m: M22) = ???
}

object M22 {
  def rotated(rad: Double): M22 = {
    val s = Math.sin(rad)
    val c = Math.cos(rad)
    M22(s, -c, s, c)
  }

  def identity(): M22 = {
    M22(1, 0, 0, 1)
  }
}
