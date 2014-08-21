package exam

object Q1 {
  import utils.io.IOUtil._
  def main(args: Array[String]) {
    val data = fileLines("data/program.txt")
    val numSemi = data.map(_.count(_ == ';')).sum
    println(numSemi)
  }
}