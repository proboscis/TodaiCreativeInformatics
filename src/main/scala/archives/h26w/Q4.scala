package archives.h26w

/**
 * @author apex
 */
object Q4 {
  def fib(i:BI,j:BI):Stream[BI] = i #:: fib(j, i+j)

  case class BI(registers:Array[Byte]){
    def +(v:BI) = {
      if(v.registers.length != registers.length) throw new RuntimeException("invalid size:"+(this,v))
      var i = 0
      val l = registers.length
      var c = 0 //carry
      val sr = registers
      val vr = v.registers
      val nr = Array.fill(l)(0.toByte)
      while(i < l){
        val a = sr(i)
        val b = vr(i)
        var res = a + b + c
        if(res > 9){
          c = 1
          res -= 10
        }else {
          c = 0
        }
        nr(i) = res.toByte
        i += 1
      }
      BI(nr)
    }
    override def toString: String = registers.reverse.mkString
  }
  object BI{
    def apply(str:String):BI = {
      BI(str.map{
        c => (c - '0').toByte
      }.reverse.toArray)
    }
  }
  def main(args: Array[String]) {
    val one = BI("00000000000000000000000000000001")
    println(fib(one,one)(140))
  }
}
