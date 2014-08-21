package utils.algorithm

import utils.Memo

/**
 * @author glyph
 */
object DP {
  val items = (2,3)::(1,2)::(3,4)::(2,2)::Nil
  type Weight = Int
  type Value = Int
  // f(x,y) = f(x-1,y-1)のストリームを作るには?
  //f(x) = f(x) + 1 のストリームでは?, g:Int=>Intのが必要
  //fib(x) = fib(x - 2) + fib(x - 1)
  def fib(x1:Int,x2:Int):Stream[Int] = (x1+x2)#::fib(x2,x2+1)

  def mkF(x:Int,next:Int=>Int):Stream[Int] = x#::mkF(next(x))
  def mkF(x:Int):Stream[Int] = x#::mkF(x+1)
  def mkStream[T](x:T,f:T=>T):Stream[T] = x#::mkStream[T](f(x),f)
  // i need two dimensional stream
  /*
  val dp:Stream[Stream[Int]] = Stream.tabulate(10,10){
    (x,y) => println(x,y)
      (x,y) match {
      case (x, 0) => 1
      case (x, 1) => 1
      case (0, y) => 1
      case (1, y) => 1
      case (x, y) => dp(x - 1)(y - 1) + dp(x - 2)(y - 2)
    }
  }
*/
  val dp2:((Int,Int)) => Int = Memo.apply[(Int,Int),Int]{
    case (x,y) if x == 0 || x == 1 || y == 0 || y == 1 => 1
    case (x,y) => dp2((x-1,y-1)) + dp2((x-2,y-2))
  }
  def main(args: Array[String]) {
    println(dp2((3,3)))
  }
}
