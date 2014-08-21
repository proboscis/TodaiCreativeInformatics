package archives

import utils.algorithm.Graphs
import utils.algorithm.Graphs._

import scala.collection.mutable
import scala.io.StdIn
import scala.util.Random

/**
 * @author glyph
 */
object H23W {
  val board1 = Seq(
    1, -1, 0,
    1, -1, -1,
    1, 1, 0
  )
  val board2 = Seq(
    -1, -1, 0,
    1, 1, 1,
    0, -1, 0
  )
  val board3 = Seq(
    -1, -1, 0,
    1, -1, 0,
    1, 1, -1
  )
  val board4 = Seq(
    -1, 1, -1,
    -1, 1, 1,
    1, -1, 1
  )
  val masks =
    Seq(0, 1, 2) :: Seq(3, 4, 5) :: Seq(6, 7, 8) ::
      Seq(0, 3, 6) :: Seq(1, 4, 7) :: Seq(2, 5, 8) ::
      Seq(0, 4, 8) :: Seq(2, 4, 6) :: Nil
  val boards = board1 :: board2 :: board3 :: board4 :: Nil
  type IntBoard = Seq[Seq[Int]]
  type CharBoard = Seq[Seq[Char]]
  type Board = Seq[Int]

  def intToChar(i: Int) = i match {
    case 1 => 'O'
    case -1 => 'X'
    case 0 => '-'
  }

  def resultToMsg(rOpt: Option[Int]) = rOpt match {
    case Some(r) => r match {
      case 1 => "X win!"
      case -1 => "O win!"
      case 0 => "Draw!"
    }
    case None => "not finished yet"
  }

  def toCharBoard(b: IntBoard) = b.map(_.map {
    case 1 => 'O'
    case -1 => 'X'
    case 0 => '-'
  })

  def boardText(board: CharBoard) = board.map(_.mkString).mkString("\n")

  def main(args: Array[String]) {
    boards map (_.grouped(3).toSeq) map toCharBoard map boardText foreach println
  }

  def bIndex(x: Int, y: Int) = y * 3 + x

  def checkTurn(b: Board) = if (b.sum > 0) -1 else 1

  def updatedBoard(x: Int, y: Int, b: Board): Board = {
    val hand = checkTurn(b)
    b.updated(bIndex(x, y), hand)
  }

  def updatedBoard(i: Int, b: Board): Board = {
    val hand = checkTurn(b)
    b.updated(i, hand)
  }

  def checkResult(b: Board) = {
    val sums = masks map {
      mask => mask.map(b).sum
    }
    val winX = sums.contains(-3)
    val winY = sums.contains(3)
    val draw = !b.contains(0)
    (winX, winY, draw) match {
      case (true, _, _) => Some(-1)
      case (_, true, _) => Some(1)
      case (_, _, true) => Some(0)
      case _ => None
    }
  }

  def getSums(b: Board) = masks map {
    mask => mask.map(b).sum
  }

  /*
  幅優先探索(v)
    v に訪問済みの印を付ける
    v をキューに追加
    while (キューに要素を含むなら)
        v ← キューから取り出す
        v を処理
        for each (v に接続していて かつ 未訪問の頂点 i)
            i に訪問済みの印を付ける
            i をキューに追加
   */

  /*
  深さ優先探索(v)
    S ← 空のスタック
    v に訪問済みの印を付ける
    v を S に積む
    while (S が空ではない)
        v ← S から取り出す
        v を処理する
        for each (v に接続していて かつ 未訪問の頂点 i)
            i に訪問済みの印を付ける
            i を S に積む
   */
  def predictResults2(b: Board): Seq[Int] = {
    val result = mutable.Buffer[Int]()
    val stack = mutable.Stack[Board](b)
    val visited = mutable.Set[Board](b)
    while (!stack.isEmpty) {
      val board = stack.pop()
      checkResult(board) match {
        case Some(r) => result += r
        case None =>
          for (f <- places(board)
            .map(updatedBoard(_, board))
            .filterNot(visited)) {
            visited += f
            stack.push(f)
          }
      }
    }
    result
  }
  import Graphs._
  def gameTree(b:Board) = dfsVertices2((b,List[Int](),None:Option[Int]))(_._1) {
    case (board, hands, res) => res match {
      case Some(r) => Nil
      case None => places(board).map {
        p => val nb = updatedBoard(p, board)
          (nb, p :: hands, checkResult(nb))
      }
    }
  }
  def predictResultStat(b: Board, i: Int): Map[Int, Int] =
    predictResults2(updatedBoard(i, b)).groupBy(identity)
      .mapValues(_.size).withDefaultValue(0)

  def places(b: Board) = b.zipWithIndex.collect {
    case (p, i) if p == 0 => i
  }

  implicit class BoardOps(val b: Board) extends AnyVal {
    def result = checkResult(b)

    def predictResult = predictResults2(b)

  }

  def getBetterHand(b: Board) = {
    val turn = checkTurn(b)
    places(b).map(pos => pos -> predictResultStat(b, pos)).sortBy {
      case (pos, stats) => stats(turn) - stats(-turn)
    }.reverse.head._1
  }

  def getHumanHand() = {
    val line = readLine()
    val x = line(0) - '0'
    val y = line(1).toUpper.toInt - 'A'
    (x, y)
  }

  def getCPUHand(b: Board) = {
    val empties = b.zipWithIndex.filter {
      case (p, i) => p == 0
    }.map(_._2)
    val i = Random.shuffle(empties).head
    (i % 3, i / 3)
  }
}

object Game {

  import archives.H23W._

  //Case class seems to be a better solution

  def main(args: Array[String]) {
    //ScalaのStreamとIteratorでStateマシンを扱うと、時間ばかり取られそうである。

    //so, stop making state machine with immutable values! ok.
    q6()

  }


  def q6() {
    var board = Seq.fill(9)(0)
    while (board.result.isEmpty) {
      val i = getBetterHand(board)
      board = updatedBoard(i, board)
      println(gameTree(board))
      println(board.grouped(3).map(seq => seq.map(intToChar).mkString).mkString("\n"))
      val result = resultToMsg(checkResult(board))
      println(result)
    }
  }

  def q5() {
    var board = Seq.fill(9)(0)
    var finished = false
    while (!finished) {
      Thread.sleep(1000)
      val (x, y) = getCPUHand(board)
      println(x, y)
      val i = bIndex(x, y)
      board = updatedBoard(x, y, board)
      println(gameTree(board))
      println(board.grouped(3).map(seq => seq.map(intToChar).mkString).mkString("\n"))
      val result = checkResult(board) match {
        case Some(r) => finished = true
          r match {
            case -1 => "X win!"
            case 1 => "O win!"
            case 0 => "draw !"
          }
        case None => "...."
      }
      println(result)
    }
  }


  def q4() {

    var board = Seq.fill(9)(0)
    var finished = false
    while (!finished) {
      Thread.sleep(1000)
      val (x, y) = getCPUHand(board)
      println(x, y)
      board = updatedBoard(x, y, board)
      println(board.grouped(3).map(seq => seq.map(intToChar).mkString).mkString("\n"))
      val result = checkResult(board) match {
        case Some(r) => finished = true
          r match {
            case -1 => "X win!"
            case 1 => "O win!"
            case 0 => "draw !"
          }
        case None => "...."
      }
      println(result)
    }
  }


  def q3() {
    def getHand(b: Board): (Int, Int) = checkTurn(b) match {
      case 1 => getHumanHand()
      case -1 => getCPUHand(b)
    }

    var board = Seq.fill(9)(0)
    var finished = false
    while (!finished) {
      val (x, y) = getHand(board)
      println(x, y)
      board = updatedBoard(x, y, board)
      println(board.grouped(3).map(seq => seq.map(intToChar).mkString).mkString("\n"))
      val result = checkResult(board) match {
        case Some(r) => finished = true
          r match {
            case -1 => "X win!"
            case 1 => "O win!"
            case 0 => "draw !"
          }
        case None => "...."
      }
      println(result)
    }
  }

  def q2() {
    var board = Seq.fill(9)(0)
    var finished = false
    while (!finished) {
      val line = readLine()
      val x = line(0) - '0'
      val y = line(1).toUpper.toInt - 'A'
      println(x, y)
      board = updatedBoard(x, y, board)
      println(board.grouped(3).map(seq => seq.map(intToChar).mkString).mkString("\n"))
      val result = checkResult(board) match {
        case Some(r) => finished = true
          r match {
            case -1 => "X win!"
            case 1 => "O win!"
            case 0 => "draw !"
          }
        case None => "...."
      }
      println(result)
    }
  }
}
