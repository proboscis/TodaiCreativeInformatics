package exam

/**
 * @author apex
 */
object Q2 {
  import utils.io.IOUtil._
  def main(args: Array[String]) {
    val data = fileLines("data/program.txt")
    data.zipWithIndex.map(_.swap).foreach(println)
  }
}
