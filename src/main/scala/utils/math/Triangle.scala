package utils.math

/**
 * @author glyph
 */
case class Triangle(a: V2, b: V2, c: V2) {
  def contains(p: V2): Boolean = {
    val pa = p - a
    val pb = p - b
    val pc = p - c
    (pa crs pb) >= 0.0f &&
      (pb crs pc) >= 0.0f &&
      (pc crs pa) >= 0.0f
  }

  def edges = Seq(a, b, c, a).sliding(2).map {
    case p1 :: p2 :: Nil => (p1, p2)
  }.toSeq
}

