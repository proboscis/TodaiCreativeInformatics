package utils.io

import scala.io.{StdIn, Source}

/**
 * @author glyph
 */
object IOUtil {
  /**
   * iterator that returns each line of file and closes the file after iteration.
   * @param filename name of the file in any form
   * @return iterator of the lines of file.
   */
  def fileLines(filename:String):Iterator[String] = new Iterator[String]{
    val src = Source.fromFile(filename)
    val lines = src.getLines()
    var closed = false
    override def hasNext: Boolean = (!closed) && lines.hasNext
    override def next(): String = {
      val line = lines.next()
      if(!lines.hasNext){
        src.close()
        closed = true
      }
      line
    }
  }
  def fileChars(filename:String):Iterator[Char] = new Iterator[Char]{
    val src = Source.fromFile(filename)
    var closed = false

    override def hasNext: Boolean = (!closed) && src.hasNext
    override def next(): Char = {
      val c = src.next()
      if(!src.hasNext){
        src.close()
        closed = true
      }
      c
    }
  }

  /**
   * use StdIn for readChar, readFloat.. and so on
   * Source.stdin itself is an iterator of std input
   * @return
   */
  def stdChars:Iterator[Char] = Source.stdin
}
