package exam

import utils.io.IOUtil._

/**
 * @author apex
 */
object Q6 {
  import scala.collection.{mutable => m}
  import Math._
  def diff(a:String,b:String) = {
    val (la,lb) = (a.size,b.size)
    val dp = Array.fill(la,lb)(0)
    def getDP(i:Int,j:Int) = if(i == 0) j else if (j == 0) i else
    if(i < 0 || j < 0) Int.MaxValue/2 else dp(max(i,0))(max(j,0))
    var i = 0
    while (i < la){
      var j = 0
      while (j < lb){
        if(a(i) == b(j)){
          dp(i)(j) = getDP(i-1,j-1)
        }else {
          dp(i)(j) = min(getDP(i,j-1)+1,getDP(i-1,j)+1)
        }
        j += 1
      }
      i += 1
    }
    //println(dp.map(_.mkString("\t")).mkString("\n"))
    dp(la-1)(lb-1)
    //動的計画法のやつで距離を測る
  }
  def main(args: Array[String]) {
    /*
    val a = """#if EXIT_SUCCESS != 0 || EXIT_FAILURE != 1"""
    val b = """#if EXIT_FAILURE != 1"""
    println(diff(a,b))
    */

    val lines = fileLines("data/program.txt").filter(_.size >= 20).toStream
    val distinct = lines.distinct
    for(a <- distinct){
      for( b <- distinct){
        val d = diff(a,b)
        if(d != 0 && d < 4){
          println(a,b)
        }
      }
    }
  }
}
