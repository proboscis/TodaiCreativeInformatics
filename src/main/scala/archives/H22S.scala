import utils.algorithm.WaterPool
import processing.core.PApplet
import utils.io.IOUtil

/**
 * 手続き的に書いた部分で一番バグが多かった・・・
 *
 * @author glyph
 */
case class Rect(x:Int,y:Int,w:Int,h:Int){
  def area = w*h
  def contains(r:Rect):Boolean = {
    val Rect(rx,ry,rw,rh) = r
    contains(rx,ry,rw,rh)
  }
  def contains(rx:Int,ry:Int,rw:Int,rh:Int):Boolean = {
    x <= rx && rx + rw <= x+w &&
      y <= ry && ry + rh <= y+h
  }
  def points = for(i <- x to x+w;j <- y to y+h) yield(i,j)
}
object Rect{
  def unapply(line:String):Option[Rect] = {
    val data = line.split(" ").map(_.toInt)
    val Array(x,y,w,h) = data
    Some(Rect(x,y,w,h))
  }
  def parseLine(line:String):Rect ={
    val Array(x,y,w,h) = line.split(" ").map(_.toInt)
    Rect(x,y,w,h)
  }
}

object H22S {
  import IOUtil._
  val seven = fileToRectangles("data/h22s/7.txt")
  val ten = fileToRectangles("data/h22s/10.txt")
  val thousands = fileToRectangles("data/h22s/1000.txt")
  def fileToRectangles = fileLines(_:String).map(Rect.parseLine).toArray
  def fillField(field:Array[Array[Int]],rects:Seq[Rect]) = rects.foldLeft(field){
    case(buf,r@Rect(x,y,w,h))=>{
      for(x<- x until x+w; y<- y until y+h) buf(x)(y) += 1
      buf
    }
  }
  def createField = Array.fill(1000,1000)(0)
  def q1_1 = {
    val field = fillField(createField,ten)
    field.flatten.max
  }
  def q1_2 = {
    val field = fillField(createField,ten)
    WaterPool.findArea(field,1000,1000)(_ > 0,0)
  }

  def q1(){//INCOMPLETE!
    val field = fillField(createField,ten)
    val a1 = field.flatten.max
    val areas = WaterPool.findArea(field,1000,1000)(_ > 0,0)
    val a2 = areas.size
    val a3 = areas.map(_.size).max//要素数を出すには、水たまりに箱のIDを書いておく必要がある
    val a4 = areas.map(_.size).max
    (a1::a2::a3::a4::Nil) foreach println
  }
  def q2(){
    println(thousands.map(_.area).sum)
  }
  def q3(){//INCOMPLETE!
    val field = fillField(createField,thousands)
    val a1 = field.flatten.max
    val areas = WaterPool.findArea(field,1000,1000)(_ > 0,0)
    val a2 = areas.size
    val a3 = areas.map(_.size).max//要素数
    val a4 = areas.map(_.size).max
    (a1::a2::a3::a4::Nil) foreach println
  }
  def q4(){
    //最大値の座標を全て出し、それにRを重ねる方法を全て答える。オーバーラップを考慮する
    val field = fillField(createField,thousands)
    val max = field.flatten.max
    val positions = for{
      (row,x)<-field.zipWithIndex
      (v,y)<-row.zipWithIndex if v == max
    } yield (x,y)
    //オーバーラップについては、重なっている面積を引けば良い
    //定義域を考慮してRを動かす関数を返すには
    //一つの点につきw*hのパターンがあるので
    val range = Rect(0,0,1000,1000)
    def patterns(px:Int,py:Int):Seq[(Int,Int)] = for{
      dx <- -5 until 0
      dy <- -10 until 0 if range.contains(px+dx,py+dy,5,10)
    } yield (px+dx,py+dy)
    //positions * 50の座標を得て、重複をのぞく
    val a1 = positions.flatMap{
      case(x,y) => patterns(x,y)
    }.distinct
    a1 foreach println
    println(a1.size)

    //(2)最大のクラスタを持ってきて、最大化を行なう
    val largest = WaterPool.findArea(field,1000,1000)(_ > 0,0).sortBy(_.size).last
    //largestに重なるRの置き方を全て求めて、面積増加量が最大となるものを数える
    //総当り
    val possibilities2 = largest.flatMap{
      case(x,y) =>patterns(x,y)
    }.distinct
    val increases = possibilities2.map{
      case(x,y) => Rect(x,y,5,10).points.filterNot(largest.contains)
    }.groupBy(_.size)

    val a2 = increases(increases.keySet.max)
    a2 foreach println
    println(a2.size)
  }
  def q5(){
    /**
     * C2'が9999999の時、現状と同じアルゴリズムでは10万秒程度かかる
     * 10000000*10000000 = 100000000000000 <= INT.MAX? もし違えばLongで対応する
     * 1 から 100,000,000,000,000までincrement するには 10万秒程度かかる
     * 従って、箱を置く度に最大値等を更新していく必要がある。
     * 箱の数は1000であるため、shortで地図を作る
     * 地図は1,000,000 * 1,000,000 * 16 bit = 16Tbits = 2TByteのバッファは用意できない
     * 従って箱同士の当たり判定を行なうのが適切。
     * 箱の配置に応じて空間を４分木に分割し、当たり判定を行なう
     * 後は衝突のリストに対してクラスタ面積、最大厚み等を判定すれば良い
     */
  }

  def main(args: Array[String]) {
    PApplet.main("H22View")
  }
}

class H22View extends PApplet{
  import H22S._
  val field = fillField(createField,thousands)
  override def setup(): Unit = {
    size(1000,1000)
    background(0)
    noLoop()
  }
  override def draw(): Unit = {
    println("drawing")
    var x = 0
    while ( x < 1000){
      var y = 0
      while(y < 1000){
        stroke(field(x)(y)/3f * 255f)
        point(x,y)
        y += 1
      }
      x += 1
    }
    val largest = WaterPool.findArea(field,1000,1000)(_ > 0,0).sortBy(_.size).last
    largest.foreach{
      case(x,y) => {
        stroke(255,255,100f)
        point(x,y)
      }
    }

  }
}
