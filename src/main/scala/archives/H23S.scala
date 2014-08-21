import utils.CollectionUtil
import utils.io.IOUtil
import utils.syntax.IdOps

/**
 * @author glyph
 */
object H23S {
  import IOUtil._
  import CollectionUtil._
  val regDigit = """\d{3}""".r
  def firstLine(f:String) = fileLines(f).toSeq.head
  def extract(text:Iterator[Char]):StringBuilder = {
    val buf = new StringBuilder
    val it = text.zipWithIndex
    while(it.hasNext){
      val(c,i) =it.next()
      if(c.isDigit) {
        val indexList = c :: it.next()._1 :: it.next()._1 :: Nil
        val index = indexList.mkString.toInt
        val overwrap = i - index
        if (overwrap < 6) {
          val known = buf.subSequence(index,i)
          buf ++= Stream.continually(known.toString).flatten.take(6)
        } else {
          buf append buf.subSequence(index,index+6)
        }
      } else {
        buf += c
      }
    }
    buf
  }
  def extract(str:String):StringBuilder = extract(str.iterator)
  val toDigit = "%03d".format(_:Int)
  def compress(text:String) = {
    val dict = createMap(text)
    val it = (text+"@@@@@").sliding(6).zipWithIndex
    val buf = new StringBuilder
    while(it.hasNext){
      val(token,i) = it.next()
      println(token,i)
      dict.get(token) match{
        case Some(index) if i != index => buf ++= toDigit(index);it.drop(5)
        case _ => buf += token(0)
      }
    }
    buf
  }
  def createMap(line: String) = {
    line.sliding(6).zipWithIndex.foldLeft(Map[String, Int]()) {
      case (table, kv@(token, i)) => table.get(token) match {
        case Some(index) => table
        case None => table + kv
      }
    }
  }
  def countDigitsInFile(f: String) = fileLines(f).map(getDigits(_).size).sum
  def getDigits(str: String) = regDigit.findAllIn(str)
  val c1 = "aabbba000c001008a"
  val a1 = "aabbbaaabbbacabbbaabbbacaa"
  val c2 = "aabbccdd000dd002aa"
  val a2 = "aabbccddaabbccddbbccddaa"
  def q1_1 = "aabbba000c001008a" -> "aabbba,aabbba,c,abbbaa,bbbaca,a"
  def q1dash = "aabbbaaabbbacabbbaabbbaaa" -> "aabbba000c001008a"
  def q1_2 = "aabbccddaabbccddbbccddaa" -> "aabbccdd000dd002aa"
  def q2 = Seq("c1.txt", "c2.txt") map countDigitsInFile foreach println
  def q3 = fileLines("s1.txt").map(createMap(_).size)

  def q4 = Seq("s1.txt","s2.txt").view.map(fileLines(_).map(compress)).map(_.toSeq).map {
    compressed => (compressed.takeRight(10), compressed.size)
  }.force
  def q5 = Seq("c1.txt","c2.txt") map {
   f => extract(firstLine(f).iterator)
  }

  def q6 = extractLarge(compressLarge(fileChars("s3.txt")))
  import IdOps._
  def q6_4 = Seq("s1.txt","s2.txt") map (fileChars(_) |> compressLarge) map {
    str => (str takeRight 10, str.size)
  }
  def q6_5 = Seq("c1.txt","c2.txt") map (fileChars(_) |> extractLarge)
  def compressLarge(chars:Iterator[Char]) = chars.grouped(1000).map(_.mkString).map(compress).mkString(":")
  def extractLarge(chars:Iterator[Char]) = chars.split(_==':').map(extract)
  def extractLarge(string:String) = string.split(":").map(extract).mkString
  def main(args: Array[String]) {
    println(c1)
    println(a1)
    println(extract(c1))
    println(compress(a1))
  }
}
