import java.lang.Math._

import org.jenetics._
import utils.io.IOUtil

import scala.annotation.tailrec

/**
 * @author glyph
 */
object H24 {
  lazy val Pair = """\((\d+), (\d+)\)""".r
  lazy val data1 = IOUtil.fileLines("data1.txt").map {
    case Pair(a, b) => (a.toInt, b.toInt)
  }.toSeq

  def q1 = data1.map(_._2).max

  def q2() {
    val graph = new AsciiGraph(100, 100, 30d, 30d)
    for ((x, y) <- data1) {
      graph.plot('@', x, y)
    }
    graph.show()
  }

  def q3(a: Double, b: Double) {
    val points = 0d until 30d by 1d map {
      x => (x, a * x + b)
    }
    val graph = new AsciiGraph(100, 100, 30d, 30d)
    for ((x, y) <- points) {
      graph.plot('@', x, y)
    }
    graph.show()
  }

  def q4() {
    import java.lang.Math._
    val allX = data1.map(_._1)
    val allY = data1.map(_._2)
    val allXY = data1.map {
      case (x, y) => x * y
    }
    val sumX = allX.sum
    val sumY = allY.sum
    val sumXY = allXY.sum
    val sumX2 = allX.map(pow(_, 2)).sum
    val sumY2 = allY.map(pow(_, 2)).sum
    val N = data1.size
    val a = (N * sumXY - sumX * sumY) / (N * sumX2 - pow(sumX, 2))
    val b = (sumX2 * sumY - sumXY * sumX) / (N * sumX2 - pow(sumX, 2))
    q3(a, b)
  }

  def leastSquare(points: Seq[(Double, Double)]) = {
    val allX = points.map(_._1)
    val allY = points.map(_._2)
    val allXY = points.map {
      case (x, y) => x * y
    }
    val sumX = allX.sum
    val sumY = allY.sum
    val sumXY = allXY.sum
    val sumX2 = allX.map(pow(_, 2)).sum
    val N = points.size
    val a = (N * sumXY - sumX * sumY) / (N * sumX2 - pow(sumX, 2))
    val b = (sumX2 * sumY - sumXY * sumX) / (N * sumX2 - pow(sumX, 2))
    (a, b)
  }

  def squareDiff(points: Seq[(Double, Double)], f: Double => Double) = points.map {
    case (x, y) => pow(y - f(x), 2)
  }.sum

  def linear(a: Double, b: Double) = (x: Double) => a * x + b

  def q5() {
    val generateF = (Xm: Double) => (data: Seq[(Double, Double)]) => {
      val (left, right) = data.partition(_._1 < Xm)
      val (a1, b1, lf) :: (a2, b2, rf) :: Nil = (left :: right :: Nil).map(leastSquare).map {
        case (a, b) => (a, b, linear(a, b))
      }
      val f = (x: Double) => if (x <= Xm) lf(x) else rf(x)
      (a1, b1, a2, b2, f, squareDiff(data, f))
    }
    //clime mountain
    val data: Seq[(Double, Double)] = 1d to 30d by 0.333d map (x => (x, 20 * abs(sin(x / (15d / PI)))))
    val Xm = fall(generateF(_)(data)._6, 0d, 30d)
    val (a1, b1, a2, b2, f, diff) = generateF(Xm)(data)
    val graph = new AsciiGraph(100, 100, 30d, 30d)
    graph.plot(data, '*')
    graph.plot(f, '@')
    graph.show()
    println(Xm, a1, a2, b1, b2, diff)
    //え、mxで接続？？？無理・・・
  }

  def linear(x:Double,a:Double,b:Double) = a*x + b
  def twoLinear(a1:Double,b1:Double,a2:Double,b2:Double,Xm:Double,x:Double) = {
    if(x <= Xm ){
      linear(x,a1,b1)
    }else{
      linear(x,a2,b2)
    }
  }
  def q52(){
    val data: Seq[(Double, Double)] = 1d to 30d by 0.333d map (x => (x, 20 * abs(sin(x / (30d / PI)))))

    import org.jenetics.util.{Function => F}
    val factory = Genotype.of(DoubleChromosome.of(-100d,100d,4))
    val ff = new F[Genotype[DoubleGene],java.lang.Double]{
      override def apply(value: Genotype[DoubleGene]): java.lang.Double = {
        val chromosome = value.getChromosome
        val y1 = chromosome.getGene(0).doubleValue()
        val x2 = chromosome.getGene(1).doubleValue()
        val y2 = chromosome.getGene(2).doubleValue()
        val y3 = chromosome.getGene(3).doubleValue()
        val Xm = x2
        val a1 = (y2-y1)/(x2-0d)
        val b1 = y2 - a1*x2
        val a2 = (y3-y2)/(30d-x2)
        val b2 = y3 - a2*30d
        if(0d <= Xm && Xm < 30d) {
          data.map {
            case (x, y) => {
              val d = y - twoLinear(a1, b1, a2, b2, Xm, x)
              d * d
            }
          }.sum
        } else{
          1000000000d
        }
      }
    }
    val ga = new GeneticAlgorithm[DoubleGene,java.lang.Double](factory,ff,Optimize.MINIMUM)
    ga.setStatisticsCalculator(new NumberStatistics.Calculator)
    ga.setPopulationSize(10)
    ga.setSelectors(new RouletteWheelSelector)
    ga.setAlterers(new Mutator(0.8),new SinglePointCrossover(0.06))
    ga.setup()
    ga.evolve(100000)
    show()
    def show(){
      val graph = new AsciiGraph(30, 30, 30d, 30d)
      val ch = ga.getBestPhenotype.getGenotype.getChromosome
      import scala.collection.JavaConverters._
      val list@y1::x2::y2::y3::Nil = ch.iterator().asScala.toList.map(_.doubleValue())
      val a1 = (y2-y1)/(x2-0d)
      val b1 = y2 - a1*x2
      val a2 = (y3-y2)/(30d-x2)
      val b2 = y3 - a2*30d
      val Xm = (b2-b1)/(a1-a2)
      graph.plot(data, '*')
      graph.plot(twoLinear(a1,b1,a2,b2,Xm,_:Double), '@')
      graph.show()
    }
    println(ga.getBestStatistics)
  }

  def fall(f: Double => Double, start: Double, end: Double): Double = {
    @tailrec
    def fall(s: Double, e: Double): Double = {
      println(s, e)
      val r = e - s
      val dx = r / 10d
      if (dx > 0.000001d) {
        var pos = s
        var x = s
        var champ = Double.MaxValue
        while (x <= e) {
          val v = f(x)
          if (champ > v) {
            champ = v
            pos = x
          }
          x += dx
        }
        println(x)
        fall(max(start, pos - r / 4), min(end, pos + r / 4d))
      } else {
        (s + e) / 2d
      }
    }
    fall(start, end)
  }

  def main(args: Array[String]) {
    q52()
  }
}

class AsciiGraph(w: Int, h: Int, xMax: Double, yMax: Double) {
  val scaleX = w / xMax
  val scaleY = h / yMax
  val buffer = Array.fill(h, w)(' ')

  def plot(f: Double => Double, c: Char) {
    for (x <- 0d to xMax by (xMax / w)) plot(c, x, f(x))
  }

  def plot(points: Seq[(Double, Double)], c: Char) {
    for ((x, y) <- points) plot(c, x, y)
  }

  def plot(c: Char, x: Double, y: Double) = {
    val py = (scaleY * y).toInt
    val px = (scaleX * x).toInt
    if (0 <= px && px < buffer(0).size && 0 <= py && py < buffer.size) buffer(py)(px) = c
  }

  def plot(buf: Array[Array[Char]], str: String, x: Double, y: Double) = {
    val px = (scaleX * x).toInt
    val py = (scaleY * y).toInt
    str.zipWithIndex.foreach {
      case (c, i) => {
        if (py < buf.size && px < buf(py).size)
          buf(py)(px + i) = c
      }
    }
  }

  def show() {
    val extendX = buffer.map {
      ary => Array.fill(3)(' ') ++ ary
    }
    val extended = Array.fill(2, w + 3)(' ') ++ extendX
    extended.foreach {
      ary => ary(2) = '|'
    }
    extended(1) = Array.fill(w + 3)('-')
    for (i <- 0 to 30 by 10) {
      plot(extended, i.toString, i, 0)
      plot(extended, i.toString, 0, i)
    }
    extended.reverse.foreach {
      ary => println(ary.mkString)
    }
  }
}
