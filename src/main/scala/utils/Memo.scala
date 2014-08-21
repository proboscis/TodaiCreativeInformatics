package utils

import scala.collection.{mutable => m}
/**
 * @author apex
 */
/** A function memoization strategy.  See companion for various
  * instances employing various strategies.
  */
sealed trait Memo[@specialized(Int) K, @specialized(Int, Long, Double) V] {
  def apply(z: K => V): K => V
}
object Memo {
  def memo[@specialized(Int) K, @specialized(Int, Long, Double) V](f: (K => V) => K => V): Memo[K, V] = new Memo[K, V] {
    def apply(z: K => V) = f(z)
  }
  def mutableMapMemo[K, V](a: m.Map[K, V]): Memo[K, V] =
    memo[K, V](f => k => a.getOrElseUpdate(k, f(k)))
  /** Cache results in a [[scala.collection.mutable.HashMap]].
    * Nonsensical if `K` lacks a meaningful `hashCode` and
    * `java.lang.Object.equals`.
    */
  def mutableHashMapMemo[K, V]: Memo[K, V] =
    mutableMapMemo(new m.HashMap[K, V])
  def apply[K,V]:Memo[K,V] = mutableHashMapMemo[K,V]
}
object MemoTest{
  /**
   * recursive function cannot be defined as a local variable
   */
  val fib:Int=>Int = Memo.apply{
    case 0 => 0
    case 1 => 1
    case n => fib(n-2) + fib(n-1)
  }
  def main(args: Array[String]) {
    println(fib(45))
  }
}
