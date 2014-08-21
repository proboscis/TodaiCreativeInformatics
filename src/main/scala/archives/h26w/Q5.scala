package archives.h26w

/**
 * @author apex
 */
object Q5 {

  def main(args: Array[String]) {
    val a = BD(BI("12345678901234567890123456789012"),2)
    val b = BD(BI("98765432109876543210987654321098"),9)
    println(a*b)
  }
  case class BD(real:BI,exp:Int){
    def *(v:BD):BD = {
      val l = real.registers.length
      val sr = real.registers
      val vr = v.real.registers
      val mr = Array.fill(l,l*2)(0.toByte)
      var c = 0//carry
      var i = 0
      while(i < l){//b
        var j = 0
        while(j < l){//a
          val a = sr(j)
          val b = vr(i)
          var res = a * b + c
          if(res > 9){
            c = res / 10
            res %= 10
          }else {
            c = 0
          }
          mr(i)(j+i) = res.toByte
          j += 1
        }
        mr(i)(j+i) = c.toByte // make sure to add carry
        i += 1
      }
      val tr = mr.map(BI.apply).reduce(_+_)
      val reversed = tr.registers.reverse
      val values = reversed.dropWhile(_ == 0)
      val shift = values.size - l
      val r =  BI(values.take(l).reverse)
      BD(r ,exp + v.exp + shift)
    }

    override def toString: String = """%s %2d""".format(real,exp)
  }

  case class BI(registers:Array[Byte]){
    def +(v:BI) = {
      if(v.registers.length != registers.length) throw new RuntimeException("invalid size:"+(this,v))
      var i = 0
      val l = registers.length
      var c = 0 //carry
      val sr = registers
      val vr = v.registers
      val nr = new Array[Byte](l)
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
}
