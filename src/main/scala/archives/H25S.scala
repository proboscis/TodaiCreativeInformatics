package archives

import utils.io.IOUtil

import scala.collection.immutable.Stack
import scala.collection.mutable
;

/**
 * //you need a regex cheat sheet!
 * //hope this works..
 * @author glyph
 */
object H25S {

  trait Exp

  trait BinOp {
    def a: Exp

    def b: Exp
  }

  case class Symbol(name: String) extends Exp

  case class Integer(value: Int) extends Exp

  case class Add(a: Exp, b: Exp) extends BinOp

  case class Cmp(a: Exp, b: Exp) extends BinOp

  case class Jmp(a: Exp, b: Exp) extends BinOp

  case class Prn(a: Exp, b: Exp) extends BinOp

  case class Set(a: Exp, b: Exp) extends BinOp

  case class Sub(a: Exp, b: Exp) extends BinOp

  case class Bak(a: Exp, b: Exp) extends BinOp

  case class Cal(a: Exp, b: Exp) extends BinOp

  case class Ret(a: Exp, b: Exp) extends BinOp

  val Characters = """([a-z]+)""".r
  val Integers = """(-?\d+)""".r

  def parseExp(l: String): Exp = l match {
    case Characters(c) => Symbol(c)
    case Integers(i) => Integer(i.toInt)
  }

  def parseLine(l: String): BinOp = {
    val tokens = l.split(" ")
    val ops = tokens(0)
    val a = parseExp(tokens(1))
    val b = parseExp(tokens(2))
    ops match {
      case "ADD" => Add(a, b)
      case "CMP" => Cmp(a, b)
      case "JMP" => Jmp(a, b)
      case "PRN" => Prn(a, b)
      case "SET" => Set(a, b)
      case "SUB" => Sub(a, b)
      case "BAK" => Bak(a, b)
      case "CAL" => Cal(a, b)
      case "RET" => Ret(a, b)
    }
  }

  def q1() = IOUtil.fileLines("prog1.txt").map(parseLine).map(_.a).foreach(println)

  case class M(stack: List[Map[String, Int]], pcStack: List[Int], program: Seq[BinOp], output: List[String]) {
    def updatedStack(kv: (String, Int)) = stack.updated(stack.size - 1, stack.head + kv)
    def updatedPCStack(c: Int) = pcStack.updated(pcStack.size - 1, pcStack.head + c)
    def eval(exp: Exp): Int = exp match {
      case Symbol(name) => stack.head(name)
      case Integer(i) => i
    }
    def step: M = if (!isTerminated) {
      program(pcStack.head) match {
        case Add(a, bs@Symbol(b)) => copy(stack = updatedStack(b -> (eval(a) + eval(bs))), pcStack = updatedPCStack(1))
        case Cmp(a, b) =>
          if (eval(a) == eval(b)) copy(pcStack = updatedPCStack(2))
          else copy(pcStack = updatedPCStack(1))
        case Jmp(a, _) => copy(pcStack = updatedPCStack(eval(a)))
        case Prn(a, b) => copy(pcStack = Nil, output = output :+ (eval(a), eval(b)).toString)
        case Set(Symbol(name), b) => copy(stack = updatedStack(name -> eval(b)), pcStack = updatedPCStack(1))
        case Sub(a, _) => copy(pcStack = (pcStack.head + eval(a)) :: updatedPCStack(1))
        case Bak(_, _) => copy(pcStack = pcStack.tail)
        case Cal(a, b) => copy(stack = (stack.head + ("in" -> eval(b)))::stack, pcStack = (pcStack.head + eval(a))::updatedPCStack(1))
        case Ret(a, b) => {
          //restore stack table except "in and out"
          val popped::poppedStack = stack
          val overWrapped = popped.keys.filter(poppedStack.head.keys.toSeq.contains)
          val kv = overWrapped.map(k => k -> popped(k))
          val newStack = poppedStack.updated(poppedStack.size - 1, poppedStack.head ++ kv + ("out" -> eval(a)))
          copy(stack = newStack, pcStack = pcStack.tail)
        }
      }
    } else this
    def isTerminated = pcStack.isEmpty
  }
  import collection.{mutable=>m}
  class MM(stack:m.Stack[m.Map[String,Int]],pcStack:m.Stack[Int],program:Seq[BinOp],output:m.Buffer[String]){
    def eval(exp: Exp): Int = exp match {
      case Symbol(name) => stack.head(name)
      case Integer(i) => i
    }
    def incPC(c:Int):Unit = {
      pcStack push (pcStack.pop()+c)
    }
    def setPC(c:Int):Unit = {
      pcStack.pop()
      pcStack.push(c)
    }
    def step(){
      program(pcStack.top) match{
        case Add(a, bs@Symbol(b)) => stack.top += b -> (eval(a) + eval(bs));incPC(1)
        case Cmp(a, b) =>
          if (eval(a) == eval(b))incPC(2)
          else incPC(1)
        case Jmp(a, _) => setPC(eval(a))
        case Prn(a, b) => pcStack.clear(); output += (eval(a), eval(b)).toString()
        case Set(Symbol(name), b) => stack.top += (name -> eval(b)); incPC(1)
        case Sub(a, _) => incPC(1);pcStack.push(pcStack.top -1 + eval(a))
        case Bak(_, _) => incPC(-1)
        case Cal(a, b) =>
          stack.push(stack.top + ("in" -> eval(b)))
          incPC(1)
          pcStack.push(pcStack.top - 1 + eval(a))
        case Ret(a, b) =>
          //restore stack table except "in and out"
          val popped = stack.pop()
          val overWrapped = popped.keys.filter(stack.head.keys.toSeq.contains)
          val kv = overWrapped.map(k => k -> popped(k))
          stack.head ++= kv
          stack.head += ("out" -> eval(a))
          pcStack.pop()
      }
    }
    def isTerminated = pcStack.isEmpty
  }

  def main(args: Array[String]) {
    val program =
      """SET x 1
        |SET y 0
        |ADD x y
        |ADD 1 x
        |CMP x 10
        |JMP -3 0
        |PRN x y
      """.stripMargin
    program.lines foreach println
    val am = M(List(Map.empty), List(0), program.lines.map(parseLine).toSeq, Nil)
    def record(m: M): Stream[M] = m #:: record(m.step)
    val (process, results) = record(am).span(!_.isTerminated)
    process foreach println
    println(results)
  }
}
