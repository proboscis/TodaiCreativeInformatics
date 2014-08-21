package archives.h26w

/**
 * @author apex
 */
object Q3 {
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
    val a = "00123456789012345678901234567890"
    val b = "00987654321098765432109876543210"
    val res = BI(a) + BI(b)
    println(BigInt(a) + BigInt(b))
    println(res)
  }
}
