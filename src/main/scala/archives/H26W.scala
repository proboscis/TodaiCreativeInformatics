package archives

import utils.algorithm.Karnaugh
import utils.syntax.IdOps

import scala.util.parsing.combinator._
import IdOps._

/**
 * @author glyph
 */
object H26W {
  def dp(a:Int,b:Int):Int = {
    val w = 1 to 11
    val v = 1 to 11
    val buf = Array.fill(a+1,b+1)(-1)
    // i番目までの荷物でj以下になるようにとる
    def in(i:Int,j:Int):Int = {
      if(i < 0 || j < 0) 0 else
      if(buf(i)(j) >= 0) buf(i)(j)
      else {
        buf(i)(j) = if(w(i) <= j ) Math.max(in(i-1,j),in(i-1,j-w(i))+v(i)) else in(i-1,j)
        buf(i)(j)
      }
    }
    val res = in(a,b)
    buf.map{
     _.mkString("\t")
    }.mkString("\n") |> println
    res
  }

  def fib:Stream[Long] = {
    def fib(a:Long,b:Long):Stream[Long] = (a+b)#::fib(b,a+b)
    1#::1#::fib(0,1)
  }
  def bigFib:Stream[BigInt] = {
    def bigFib(a:BigInt,b:BigInt):Stream[BigInt] = (a+b)#::bigFib(b,a+b)
    BigInt(1)#::BigInt(1)#::bigFib(0,1)
  }
  def myFib:Stream[BI] = {
    def myFib(a:BI,b:BI):Stream[BI] = (a+b)#::myFib(b,a+b)
    BI("1")#::BI("1")#::myFib(BI("0"),BI("1"))
  }
  def q1() = println(fib(10))
  def q2() = println(fib(50))
  def q3() = println(bigFib(140))
  def sqrt(x:BigDecimal) = BigDecimal(Math.sqrt(x.doubleValue()))
  def phi = BigDecimal(1)

  def main(args: Array[String]) {
    println(dp(10,10))

  }

  case class BI(registers:Array[Int] = new Array[Int](32)){
    override def toString = registers.mkString.reverse
    def + (v:BI):BI = {
      val n = new BI
      val nr = n.registers
      val sr = registers
      var i = 0
      var inc = 0
      while(i < 32){
        val r = sr(i)+v.registers(i)+inc
        inc = r / 10
        nr(i) = r % 10
        i += 1
      }
      n
    }
  }
  object BI{
    def apply(str:String):BI = {
      val src = "%032d".format(str.toInt).reverse
      val r = BI()
      var i = 0
      while(i < 32){
        r.registers(i) = src(i) - '0'
        i+=1
      }
      println(r)
      r
    }
  }
}

object RP extends RegexParsers {
  trait Exp
  case class Parenthesis(exp: Exp) extends Exp{
    override def toString = "("+exp+")"
  }
  case class S(name: String) extends Exp {
    override def toString = name
  }
  case class And(a: Exp, b: Exp) extends Exp{
    override def toString = a + "&"+b
  }
  case class Or(a: Exp, b: Exp) extends Exp {
    override def toString = a + "+"+b
  }
  case class Not(exp: Exp) extends Exp{
    override def toString = "!"+exp
  }
  def symbol:Parser[S] = "[a-z]+".r ^^{name => S(name)}
  def expr:Parser[Exp] = term ~ rep("+" ~ term) ^^ {
    case e~list => (e/:list){
      case (l,t~r)=>Or(l,r)
    }
  }
  def term:Parser[Exp]= factor ~ rep("&" ~ factor) ^^ {
    case e~list => (e/:list){
      case (l,t~r)=>And(l,r)
    }
  }
  def factor:Parser[Exp] = not | par | symbol
  def not:Parser[Not] = "!"~>(par | symbol) ^^{e=>Not(e)}
  def par:Parser[Parenthesis] = "("~>expr<~")" ^^{e => Parenthesis(e)}
  //parse with most priority must come at the bottom
  def parse(line:String) = parseAll(expr,line)
  def main(args: Array[String]) {
    println(Karnaugh.parse("!(a&b)&c+b&a"))
  }
}




