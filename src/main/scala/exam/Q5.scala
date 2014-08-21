package exam

import utils.io.IOUtil._

/**
 * @author apex
 */
object Q5 {
  import scala.collection.{mutable => m}
  import Math._
  def diff(a:String,b:String) = {
    val l = max(a.size,b.size)
    val na::nb::Nil = (a::b::Nil).map(_.padTo(l," "))
    na.zip(nb).collect{
      case (a,b) if a != b => 1
    }.sum
  }
  def main(args: Array[String]) {
    val lines = fileLines("data/program.txt").filter(_.size >= 20).toStream
    val distinct = lines.distinct
    for(a <- distinct){
      for(b <- distinct){
        val d = diff(a,b)
        if(d != 0 && d < 4){
          println(a,b)
        }
      }
    }
  }
}
