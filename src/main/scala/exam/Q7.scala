package exam

import utils.algorithm.UnionFind
import utils.io.IOUtil._

object Q7 {
  def main(args: Array[String]) {
    //４行以上のセットで重複を探す
    //4行の行セットを作り、それでマッチを探す。
    //見つかったら、見つかった行から更にマッチが続くか探す
    val lines = fileLines("data/program.txt").toArray
    val groups = lines.sliding(4).toStream.map(_.mkString("\n")).zipWithIndex
    val duplicatedMap = groups.groupBy{
      case (g,index) => g
    }.collect{
      case (g,dup) if dup.size > 1 => g -> dup.map(_._2).toList
    }
    val matches = duplicatedMap.flatMap{
      case (key,indices) =>
        indices.combinations(2).map{
          case i::j::Nil =>
            lines.drop(i).zip(lines.drop(j)).takeWhile{
              case (li,lj) => li == lj
            }.unzip._1.mkString("\n")
        }.toList
    }
    /**
     * これだと、重複セットがかぶってしまう可能性があるがどうするか？
     *
     * kbcde~abcde~~~abcde~~~から検索すると
     * abcde == abcde
     * bcde == bcde
     * のセットが帰ってきてしまう。
     * 数列から、４連続以上で重複する組を探してくるプログラムを考えなければならない。
     * 最後にabcde とbcdeがあったとき、abcdeをとるようにすれば一応答えにはなるが・・
     * abcdeとマッチするがbcdefとマッチしない組は存在する。
     * 語尾が小さい方と同じなら同値とする
     * 総当りでチェックするか・・・
     * union木を使う?
     */
    val uf = new UnionFind[Seq[String]]
    val matchedLines = matches.map(_.lines.toSeq)
    for(mi <- matchedLines){
      for(mj <- matchedLines){
        if(mi.size != mj.size){
          val (s,l) = if(mi.size < mj.size) (mi,mj) else (mj,mi)
          val prefix = s.reverse.zip(l.reverse).takeWhile{
            case (sl,ll) => sl == ll
          }.reverse.unzip._1
          if (prefix == s){
            //s == l
            uf.unify(s,l)
          }
        }
      }
    }
    val roots = uf.parents.keys.map{
      lines => uf.find(lines)
    } ++ matchedLines.filterNot(uf.parents.contains)//union判定されなかったものをくっつける
    roots.map(_.mkString("\n")) foreach println
    //しかし、これだとabcde != kbcde != bcdeに対応できていない・・・

    /*
    //思い出した、しゃくとり法か？
    {
      var i = 0
      val L = lines.length
      while(i < L){
        var j = i+1
        val bufA = scala.collection.mutable.ArrayBuffer[String](lines(i))
        while(j < L){
          bufA += lines(j)
          //ここまででi~jのバッファ
          //j +1 から
          var k = j + 1
          while(k < L){
            var l = k +1
            val bufB = scala.collection.mutable.ArrayBuffer[String](lines(k))
            while(l < L){
              bufB += lines(l)
              l += 1
            }
            k += 1
          }
          j += 1
        }
        i +=1
      }
    }
   */
  }
}
