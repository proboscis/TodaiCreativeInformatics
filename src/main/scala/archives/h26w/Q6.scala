package archives.h26w

import archives.h26w.Q5.BD

import scala.collection.immutable.Range

/**
 * @author apex
 */
object Q6 {
  def fib(i:BI,j:BI):Stream[BI] = i #:: fib(j, i+j)

  def main(args: Array[String]) {
    println(sqrt(BigDecimal("5")))
    println(BigDecimal("2.2367")*BigDecimal("2.2367"))
  }

  def sqrt(x: BigDecimal): BigDecimal = {
    var t = BigDecimal("0")
    var dt = BigDecimal("1E-5")
    while(x - t*t > BigDecimal("1E-32")){
      t += dt
    }
    t
  }

  def phi: BD = {
    /*
    val one = BD(BI("1"), 0)
    val five = BD(BI("5"), 0)
    val half = BD(BI("5"),-1)
    (one + sqrt(five)) * half
    */
    ???
  }

  case class BD(real: BI, exp: Int) {
    def compare(v: BD): Int =if(exp-v.exp == 0){
      real.compare(v.real)
    } else exp - v.exp
    def +(v: BD):BD = {
      compare(v) match {
        case b if b > 0 => BD(real + (v.real >> (exp-v.exp)),exp)
        case b if b < 0 => BD(v.real + (real >> (v.exp - exp)),v.exp)
        case _ => BD(real + v.real,exp)
      }
    }
    def *(v: BD): BD = {
      val l = real.registers.length
      val sr = real.registers
      val vr = v.real.registers
      val mr = Array.fill(l, l * 2)(0.toByte)
      var c = 0 //carry
      var i = 0
      while (i < l) {
        //b
        var j = 0
        while (j < l) {
          //a
          val a = sr(j)
          val b = vr(i)
          var res = a * b + c
          if (res > 9) {
            c = res / 10
            res %= 10
          } else {
            c = 0
          }
          mr(i)(j + i) = res.toByte
          j += 1
        }
        mr(i)(j + i) = c.toByte // make sure to add carry
        i += 1
      }
      val tr = mr.map(BI.apply).reduce(_ + _)
      val reversed = tr.registers.reverse
      val values = reversed.dropWhile(_ == 0)
      val shift = values.size - l
      val r = BI(values.take(l).reverse)
      BD(r, exp + v.exp + shift)
    }

    override def toString: String = """%s %2d""".format(real, exp)
  }

  case class BI(registers: Array[Byte]) {
    //符号付き足し算引き算...
    def compare(v: BI): Int = {
      var i = 0
      val l = registers.length
      var flag = 0
      while (i < l && flag == 0) {
        flag = registers(i) - v.registers(i)
        i += 1
      }
      flag
    }
    def >>(n: Int): BI = {
      val l = registers.length
      val nr = new Array[Byte](l)
      BI((new Array[Byte](n) ++ registers.reverse.take(l - n)).reverse)
    }

    def <<(n: Int): BI = {
      val l = registers.length
      val nr = new Array[Byte](l)
      BI(registers.reverse.drop(n) ++ new Array[Byte](n) reverse)
    }

    def +(v: BI) = {
      if (v.registers.length != registers.length) throw new RuntimeException("invalid size:" +(this, v))
      var i = 0
      val l = registers.length
      var c = 0 //carry
      val sr = registers
      val vr = v.registers
      val nr = new Array[Byte](l)
      while (i < l) {
        val a = sr(i)
        val b = vr(i)
        var res = a + b + c
        if (res > 9) {
          c = 1
          res -= 10
        } else {
          c = 0
        }
        nr(i) = res.toByte
        i += 1
      }
      BI(nr)
    }

    override def toString: String = registers.reverse.mkString
  }

  object BI {
    def apply(str: String): BI = {
      val header = new Array[Byte](32 - str.length)
      val footer = str.map {
        c => (c - '0').toByte
      }.reverse.toArray
      BI(footer ++ header)
    }
  }

}
