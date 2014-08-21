package utils.math

import java.lang.Math._

/**
 * @author glyph
 */
case class V2(x: Double, y: Double) {
  def +(v: V2) = V2(x + v.x, y + v.y)

  def -(v: V2) = V2(x - v.x, y - v.y)

  def rot(rad: Double) = {
    val s = sin(rad)
    val c = cos(rad)
    V2(x * c - y * s, x * s + y * c)
  }

  def len = sqrt(x * x + y * y)
  def len2 = x*x + y*y

  def *(s: Double) = V2(x * s, y * s)

  def /(s: Double) = V2(x / s, y / s)

  def dot(v: V2) = V2(x * v.x, y * v.y)

  def crs(v: V2) = x * v.y - y * v.x
  def nor = this * (1d / len)
}
