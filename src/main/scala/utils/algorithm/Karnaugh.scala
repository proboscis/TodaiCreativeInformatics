package utils.algorithm

import archives.RP

/**
 * Worthless!!!
 */
object Karnaugh{
  import archives.RP.{Or, And, Parenthesis, Not,S,Exp}
  def apply(size:Int):Karnaugh = new Karnaugh(newMat(size))
  def apply(k:Karnaugh):Karnaugh = new Karnaugh(k.copyMat)
  def apply(size:Int,init:Int):Karnaugh ={
    val r = Karnaugh(size)
    var x = 0
    while(x < size){
      r.matrix(init)(x) = 1
      x += 1
    }
    r
  }
  val DC = -1
  val or = (a:Int,b:Int) => {
    if( a == DC) b
    else if(b == DC) a
    else a|b
  }
  val and = (a:Int,b:Int)=> {
    if(a == DC) b
    else if(b == DC) a
    else a&b
  }
  val not = (a:Int)=>{
    if(a == DC) DC
    else if(a > 0) 0
    else 1
  }
  def newMat(size:Int) = Array.fill(size,size)(DC)

  def parse(line:String){
    val tree = RP.parse(line).get
    val names = symbols(tree).distinct
    val(table,matrix) = symbolsToMatrix(names)
    val size = table.size
    def eval(t:Exp):Karnaugh = t match{
      case Not(exp) => !eval(exp)
      case Parenthesis(exp)=> eval(exp)
      case And(a,b)=>eval(a) & eval(b)
      case Or(a,b)=> eval(a) | eval(b)
      case S(name)=> Karnaugh(size,table(name))
    }
    val k = eval(tree)
    k.matrix.foreach{
      line => println(line.mkString(","))
    }
  }

  def symbols(t:Exp):Seq[String] = t match{
    case Not(exp) => symbols(exp)
    case Parenthesis(exp)=>symbols(exp)
    case And(a,b)=>symbols(a)++symbols(b)
    case Or(a,b)=>symbols(a)++symbols(b)
    case S(name)=> name::Nil
  }
  def symbolsToMatrix(symbols:Seq[String]):(Map[String,Int],Array[Array[Int]]) ={
    val table = symbols.zipWithIndex.toMap
    val matrix = Array.fill(table.size,table.size)(0)
    (table,matrix)
  }
  def process(dst:Array[Array[Int]],src:Array[Array[Int]])(op:(Int,Int)=>Int){
    val l = dst.size
    var x = 0
    while(x < l){
      var y = 0
      while(y < l){
        dst(x)(y) = op(dst(x)(y),src(x)(y))
        y += 1
      }
      x += 1
    }
  }
  def replMat(tgt:Array[Array[Int]])(op:Int=>Int){
    val l = tgt.size
    var x = 0
    while(x < l){
      var y = 0
      while(y < l){
        tgt(x)(y) = op(tgt(x)(y))
        y += 1
      }
      x += 1
    }
  }
}

/**
 * this don't work well
 * @param matrix
 */
class Karnaugh(val matrix:Array[Array[Int]]){
  import Karnaugh._
  def |(k:Karnaugh):Karnaugh = {
    val r = copyMat
    process(r,k.matrix)(or)
    new Karnaugh(r)
  }
  def &(k:Karnaugh):Karnaugh ={
    val r = copyMat
    process(r,k.matrix)(and)
    new Karnaugh(r)
  }
  def unary_! :Karnaugh = {
    val r = copyMat
    replMat(r)(not)
    new Karnaugh(r)
  }
  def copyMat = {
    val l = matrix.size
    val r = newMat(l)
    process(r,matrix)(or)
    r
  }
}