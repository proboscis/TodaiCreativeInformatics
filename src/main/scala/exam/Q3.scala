package exam

import utils.io.IOUtil._

/**
 * @author apex
 */
object Q3 {
  def main(args: Array[String]) {
    val lines = fileLines("data/program.txt")
    lines.sliding(2).map(_.toList).collect{
      case l1::l2::Nil if l1 == l2 => l1
    }.foreach(println)
  }
}
