package archives

/**
 * @author glyph
 */
object H21S {

  //120分てとこか・・・ => 150分以内だからok!
  //rotation => 24
  //q3-1 xとyで木を作り、UL,FU,RU,BU,LDをリストアップ
  //q3-2 24通りの置き直しは手で書き出し、xとyの木から、マッチするものを幅優先探索
  //val replaced = "y1(x),y2(z),y3(x),x1(y),x3(y),non(z)"
  case class FCube(D: Int = 0, B: Int = 1, L: Int = 2, U: Int = 3, F: Int = 4, R: Int = 5) {
    def rotate(dir: Int): FCube = dir match {
      case 0 => FCube(B, U, L, F, D, R) //X(FR)
      case 1 => FCube(R, B, D, L, F, U) //Y(UB)
      case 2 => FCube(D, L, F, U, R, B) //Z
    }
  }

  import utils.algorithm.Graphs._

  def bfsCube(src: FCube) = bfsVertices2(src -> List[Int]())(_._1) {
    case (cube, path) => (0 to 1).map {
      i => cube.rotate(i) -> (i :: path)
    }
  }

  def bfsInverse(from: FCube) = {
    val target = FCube()
    bfsCube(from).find {
      case (cube, path) => cube == target
    }.get._2
  }

  def q4_1() {
    /**
     * 面は24要素のInt配列として定義する
     * 置換は、現在のInt配列を受け取り回転後の新しい配列を返す関数として定義する
     */
  }

  val TU: Int => Int = Map(5 -> 6, 6 -> 9, 9 -> 10, 10 -> 16, 16 -> 17, 17 -> 20, 20 -> 21, 21 -> 5, 12 -> 15, 15 -> 14, 14 -> 13, 13 -> 12).withDefault(identity)
  val TR: Int => Int = Map(13 -> 14, 14 -> 17, 17 -> 18, 18 -> 0, 0 -> 1, 1 -> 4, 4 -> 5, 5 -> 13, 20 -> 23, 23 -> 22, 22 -> 21, 21 -> 20).withDefault(identity)
  val TF: Int => Int = Map(20 -> 14, 14 -> 15, 15 -> 10, 10 -> 11, 11 -> 3, 3 -> 0, 0 -> 23, 23 -> 20, 19 -> 16, 16 -> 17, 17 -> 18, 18 -> 19).withDefault(identity)
  val I = (0 until 24).toArray
  val IU = I map TU
  val IR = I map TR
  val IF = I map TF

  case class Cube(p: Array[Int] = I) {
    def U: Cube = Cube(IU map p)

    def R: Cube = Cube(IR map p)

    def F: Cube = Cube(IF map p)

    def rotations = (IU :: IR :: IF :: Nil).map(ary => Cube(ary map p))

    override def toString = p.mkString
  }

  def q4_2() {
    val c = Cube()
    (c.U :: c.R :: c.F :: Nil) foreach println
  }
  def main(args: Array[String]) {
    println("all cubes")
    val cubes = bfsCube(FCube()).map(_._1).toSeq
    println("all inverses")
    val inverses = cubes.map {
      c => c -> bfsInverse(c)
    }
    cubes foreach println
    println("-------------------------")
    inverses.foreach(println)
  }
}