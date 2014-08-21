package exam

import utils.io.IOUtil._

/**
 * @author apex
 */
object Q4 {
  def main(args: Array[String]) {
    val lines = fileLines("data/program.txt").toStream
    val isDoubled = lines.groupBy(identity).collect{
      case(line,ls) if ls.size > 1 => line
    }.toSet
    val doubled = lines.filter(isDoubled)
    doubled foreach println
    println(doubled.size)
  }
}
