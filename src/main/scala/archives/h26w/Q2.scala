package archives.h26w

import utils.Memo

/**
 * @author apex
 */
object Q2 {
  def fib(i:Long,j:Long):Stream[Long] = i #:: fib(j, i+j)

  def main(args: Array[String]) {
    //fib(1,1).take(10).foreach(println)
    println(fib(1,1)(50))
  }
}
