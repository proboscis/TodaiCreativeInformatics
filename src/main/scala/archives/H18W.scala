package archives

import scala.collection.mutable.ArrayBuffer

/**
 * 2014/8/15
 * 11:16~12:06
 * 12:08 ~ 13:45 done!
 * @author apex
 */
object H18W {
  // 0                                                 10   11   12   13   14   15
  // 0    1    2    3    4    5    6    7    8    9    a    b    c    d    e    f
  // 0000 0001 0010 0011 0100 0101 0110 0111 1000 1001 1010 1011 1100 1101 1110 1111
  def swap(bits:Int,i:Int,j:Int): Int = {
    val ith = (bits & (1 << i)) >>> i
    val jth = (bits & (1 << j)) >>> j
    if((ith ^ jth) == 1){
      bits ^ ( 1 << i) ^ (1 << j)
    } else bits
  }
  def reverse(i:Int) = swap(swap(i,4,1),6,3)

  def swapBits(bits: Seq[Int]) = {
    bits
      .updated(4, bits(1))
      .updated(1, bits(4))
      .updated(6, bits(3))
      .updated(3, bits(6))
  }

  def i2b(i: Int) = Iterator.iterate(i) {
    int => int >> 1
  }.map {
    int => int & 0x1
  }.take(32).toIndexedSeq

  def b2i(seq: Seq[Int]): Int = seq.reverse.take(32).foldLeft(0) {
    (sum, bit) => (sum << 1) | bit
  }

  def q2_3() {
    val bits = (0 to 255) filter (bits => bits == reverse(bits))
    println(bits.size)
  }


  class AM(C: Array[Boolean], rId: Int) {
    val N = C.length
    val rule = i2b(rId).map(_ > 0).toArray

    def ct1(i: Int) = {
      val l = if (C((i + N - 1) % N)) 1 else 0
      val c = if (C(i)) 1 else 0
      val r = if (C((i + 1) % N)) 1 else 0
      val id = l << 2 | c << 1 | r
      rule(id)
    }

    def step() {
      var i = 0
      while (i < N) {
        C(i) = ct1(i)
        i += 1
      }
    }

    override def toString = C.map {
      case true => '#'
      case false => '.'
    }.mkString
  }

  def q3_1() {
    val init = Array.fill(100)(false)
    init(40) = true
    val am = new AM(init, 90)
    for (i <- 1 to 1000) {
      println(am)
      am.step()
    }
  }

  def q3_2() {
    val init = Array.fill(123)(false)
    (0 to 123).map(i2b).collect {
      case bits if bits.sum == 3 => b2i(bits)
    }.foreach {
      i => init(i) = true
    }
    val am = new AM(init, 99)
    for (i <- 1 to 1000) {
      println(am)
      am.step()
    }
  }

  def q3_3() {
    val buf = scala.collection.mutable.ArrayBuffer[Boolean]()
    (1 to 40).foreach {
      i =>
        buf ++= Seq.fill(i)(false)
        buf += true
    }
    println(buf.size)
    val am = new AM(buf.toArray, 129)
    for (i <- 1 to 100) {
      println(am)
      am.step()
    }
  }

  def q4_1() {
    val init = Array.fill(100)(false)
    (0 until 100).map(i2b).collect {
      case bits if bits.sum == 3 => b2i(bits)
    }.foreach {
      i => init(i) = true
    }
    val am = new AM2(init, 53)
    for (i <- 1 to 99) {
      println(am)
      am.step()
    }
  }

  class AM2(init: Seq[Boolean], rId: Int) {
    val C = ArrayBuffer(init: _*)
    val buf = ArrayBuffer(init: _*)
    val rule = i2b(rId).map(_ > 0).toArray

    def ct1(i: Int) = {
      val N = C.size
      val l = if (C((((i - 1)% N)+N)%N)) 1 else 0
      val c = if (C(i)) 1 else 0
      val r = if (C((i + 1) % N)) 1 else 0
      val id = l << 2 | c << 1 | r
      rule(id)
    }

    def step() {
      var i = 0
      val N = C.size
      if(N<3 || N > 2000) throw new RuntimeException("size out of range")
        while (i < N) {
        C(i) = ct1(i)
        i += 1
      }
      buf.clear()
      C match {
        case ArrayBuffer(true, true, true, seq@_*) => C(0) = C(N - 1)
        case _ => //ok
      }
      buf += C(0)
      (C ++ C.take(2)).sliding(3).drop(1).foreach {
        case ArrayBuffer(false, false, false) => {
          buf += false
          buf += false
        }
        case ArrayBuffer(true, true, true) => //nop
        case ArrayBuffer(a, _, _) => buf += a
      }
      C.clear()
      C ++= buf
    }

    def seqToString(seq: Seq[Boolean]): String = seq.map {
      case true => '#'
      case false => '.'
    }.mkString

    override def toString = seqToString(C)
  }

  def q4_2() {
    val buf = scala.collection.mutable.ArrayBuffer[Boolean]()
    (1 to 40).foreach {
      i =>
        buf ++= Seq.fill(i)(false)
        buf += true
    }
    val am = new AM2(buf, 250)
    try {
      for (i <- 1 to 99) {
        println(am)
        am.step()
      }
    }catch{
      case e:Throwable => println("size out of range")
    }
  }
  def main(args: Array[String]) {
    q2_3()
  }
}

/*
Q1-1
11111111 => 255
1,1,1 1 128
1,1,0 1 64
1,0,1 1 32
0,1,1 0
1,0,0 1 8
0,1,0 0
0,0,1 0
0,0,0 0
=> 232
1,1,1
1,1,0
1,0,1
0,1,1
1,0,0
0,1,0
0,0,1
0,0,0
Q2-1
状態遷移関数は256種類あるが、その中でも
和に依存するようなものは何種類あるか？
和は3,2,1,0の４通りなので,それぞれに1と0の対応をつけると2^4通りある
従って16種類
Q2-2
LとRが入れ替わっても状態遷移が同じになるということは、
100 = 001
110 = 011
が成立することを意味し、このペアは ４通り存在する
残りの4遷移に関しては、左右反転しても遷移が変化しないため、2^4 = 16
従って16*4 = 64種類存在
Q2-3

 */
