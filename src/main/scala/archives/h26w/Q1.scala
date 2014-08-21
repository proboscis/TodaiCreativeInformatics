package archives.h26w

import utils.Memo

/**
 * @author apex
 */
object Q1 {
  val fib:Long=>Long = Memo.mutableHashMapMemo{
    i => if(i <= 2) 1 else fib(i-2) + fib(i-1)
    /*
    case 1 => 1
    case 2 => 1
    case i => fib(i-1) + fib(i-2)
    */
  }
  /*
  def fib(i:Int):Int = if(i <= 2) 1 else {
    fib(i-1) + fib(i-2)
  }
  */

  def main(args: Array[String]) {
    for(i <- 1 to 50){
      println(fib(i))
    }
  }
}
