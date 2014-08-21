package archives

import utils.io.IOUtil
import utils.io.IOUtil._

/**
 * 8割 (90分)
 * つまり、150分でなら解ける！！
 * @author apex
 */
object H19S{
  import IOUtil._
  def loopUpper(c:Int):Char = ('A' + (c-'A')%26).toChar
  def loopLower(c:Int):Char = ('a' + (c-'a')%26).toChar
  def shift(src:String,n:Int) = src.map{c => if(c.isUpper) loopUpper(c+n) else if(c.isLower) loopLower(c+n) else c}
  def charCount(str:String) = str.map(_.toLower).groupBy(identity).mapValues(_.size)
  def pChar(str:String) = {
    val size = str.length
    charCount(str).mapValues(_ / size.toDouble)
  }
  trait Tree {def d:Double}
  case class Node(l:Tree,r:Tree,d:Double) extends Tree
  case class Leaf(c:Char,d:Double) extends Tree
  def toTree(map:Char Map Double):Tree = {
    val leaves = map.map{
      case (k,v) => Leaf(k,v):Tree
    }.toList
    def fold(list:List[Tree]):Tree = {
      list.sortBy(_.d) match {
        case a::b::tail => fold(Node(a,b,a.d+b.d)::tail)
        case t::Nil => t
      }
    }
    fold(leaves)
  }
  def toHuffman(tree:Tree):Map[Char,String] = {
    var map = Map.empty[Char,String]
    def dfs(t:Tree,bin:String){
      t match{
        case Node(l,r,_) => dfs(l,bin+"0");dfs(r,bin+"1")
        case Leaf(char,d) => map += char -> bin
      }
    }
    dfs(tree,"")
    map
  }
}
object Answer{
  import H19S._
  val q1Str = fileLines("data/h19s/q1.txt").mkString("\n")
  val decrypted = shift(q1Str,15)
  def q1() = {
    println(q1Str)
    println("----------")
    (1 to 25).map(shift(q1Str,_)).zipWithIndex foreach println
    println(shift("""Hmm, you seem to have found the following sentences.""",11))
  }
  def q2() = {charCount(decrypted).foreach(println)}
  def q3(){
    val tree = toTree(pChar(decrypted))
    val codes = toHuffman(tree)
    codes foreach println
  }
  def q4(){
    val p = pChar(decrypted)
    val tree = toTree(p)
    val codes = toHuffman(tree)
    val length = codes.map{
      case (char,bin) => bin.length * p(char)
    }.sum
    println(length)
  }
  def main(args: Array[String]) {
    q4()
  }
}