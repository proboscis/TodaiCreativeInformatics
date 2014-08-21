package archives

import scala.collection.immutable.Queue

/**
 * @author apex
 */
object H25W {
  /**
   * sousyajou
   * 操車場アルゴリズム
   * @param str
   * @return Queue[Char]
   */
  def toPolishNotation(str:String) = {
    val stack = collection.mutable.Stack[Char]()
    val out = collection.mutable.Queue[Char]()
    val tokens = str.replaceAll("""\s+""","").toIterator
    val priority = Map('&'->3,'+'->2,'!'->4)
    while(tokens.hasNext){
      val c = tokens.next()
      c match {
        case '(' =>stack push c
        case ')' =>{
          while(stack.top != '('){
            out enqueue stack.pop
          }
          stack.pop()
        }
        case '!' | '&' | '+' => {
          while(stack.nonEmpty && stack.top != '(' && priority(stack.top) > priority(c)){
            out enqueue stack.pop()
          }
          stack push c
        }
        case _ => out enqueue c
      }
    }
    while(stack.nonEmpty) {
      out enqueue stack.pop
    }
    out
  }
  def evalPolishNotation(poland:Iterator[Char],valueOf:Char=>Boolean):Boolean = {
    val stack = collection.mutable.Stack[Boolean]()
    while(poland.hasNext){
      poland.next() match{
        case '!' => stack.push(!stack.pop())
        case '+' => stack.push(stack.pop() | stack.pop())
        case '&' => stack.push(stack.pop() & stack.pop())
        case c => stack push valueOf(c)
      }
    }
    stack.top
  }
  //accept patterns that return 1 only
  def names(i:Int) = ('a' + i).toChar
  def index(name:Char) = name - 'a'
  def additiveNotation(function:Seq[IndexedSeq[Boolean]]) = function.map{
      case(flags) => flags.zipWithIndex.map{
        case(flag,i) => if(flag) ""+names(i) else "!"+names(i)
      }.mkString("(","&",")")
    }.mkString("+")
  //accept patterns that return 0 only
  def multiplicativeNotation(function:Seq[IndexedSeq[Boolean]]) = function.map{
    case(flags) => flags.zipWithIndex.map{
      case(flag,i) => if(flag) "!"+names(i) else ""+names(i)
    }.mkString("(","+",")")
  }.mkString("&")

  def main(args: Array[String]) {
    val input = "a&b+!c+(a&!b)"
    val poland = toPolishNotation(input)
    val TF = true::false::Nil
    val variables = for{
      a <- TF
      b <- TF
      c <- TF
    } yield Map('a'->a,'b'->b,'c'->c)
    val results = variables.map{
      table => Array.tabulate(table.size)(i => table(names(i))).toIndexedSeq -> evalPolishNotation(poland.iterator,table)
    }
    val additive = additiveNotation(results.collect{
      case(ary,b) if b => ary
    })
    val multiplicative = multiplicativeNotation(results.collect{
      case(ary,b) if !b => ary
    })

    /**
     *      a,b,c     加法標準形   乗法標準形
     *  #0 :0,0,0 => !a*!b*!c = a + b + c
     *  #1 :0,0,1 => !a*!b*c = a + b + !c
     */
    println(input)
    println(poland.mkString)
    println(additive)
    println(multiplicative)
  }
}
