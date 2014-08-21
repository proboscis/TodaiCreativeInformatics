package utils.algorithm

import scala.annotation.tailrec
import scala.collection.{mutable => m}


/**
 * this is useful to check whether connecting a and b generates a cycle or not.
 * @author glyph
 */
class UnionFind[T] {
  val parents = m.HashMap[T, T]().withDefault(identity)
  def unify(a: T, b: T) {
    val pa = find(a)
    val pb = find(b)
    if (pa != pb) parents.update(pa, pb)
  }
  @tailrec
  final def find(a: T, route: Seq[T] = Nil): T = {
    val pa = parents(a)
    if (pa == a) {
      route.foreach(parents.update(_, a))
      a
    } else {
      find(pa, route :+ pa)
    }
  }
  def apply(a: T, b: T): Boolean = find(a) == find(b)
}
