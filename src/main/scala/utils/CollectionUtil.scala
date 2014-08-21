package utils

/**
 * @author glyph
 */
object CollectionUtil {
  implicit class IteratorOps[T](val it:Iterator[T]) extends AnyVal{
    def split(cond:T=>Boolean) = Iterator.continually(it.takeWhile(cond)).takeWhile(_.nonEmpty)
  }
  def main(args: Array[String]) {
    //tests
    //beware that char is different from String! so ' ' cannot be compared against " "
    "this is a test !".iterator.split(_ != ' ').map(_.mkString) foreach println
  }

}
