package utils.algorithm

import scala.collection.mutable.ArrayBuffer

/**
 * @author glyph
 */
object WaterPool {
  def findArea(field:Array[Array[Int]],w:Int,h:Int)(cond:Int=>Boolean,replaced:Int)
  :Seq[Seq[(Int,Int)]] ={
    def replaceFieldCall(x:Int,y:Int):Seq[(Int,Int)] = {
      val buf = new ArrayBuffer[(Int,Int)]
      replaceField(x,y,buf)
      buf
    }
    def replaceField(x:Int,y:Int,buf:ArrayBuffer[(Int,Int)]){
      val stack = collection.mutable.Stack[(Int,Int)]((x,y))
      val visited = collection.mutable.Set[(Int,Int)]((x,y))
      while(!stack.isEmpty){
        val(px,py) = stack.pop()
        if(cond(field(px)(py))){
          field(px)(py) = replaced
          buf += (px->py)
          var dx = -1
          while(dx <= 1){
            var dy = -1
            while(dy <= 1){
              val nx = px + dx
              val ny = py + dy
              if(0 <= nx && nx < w && 0 <= ny && ny <= h){
                if(!visited((nx,ny))) {
                  stack.push((nx, ny))
                  visited += (nx -> ny)
                }
              }
              dy += 1
            }
            dx += 1
          }
        }
      }
    }
    val buf =new ArrayBuffer[Seq[(Int,Int)]]()
    var x = 0
    while(x < w){
      var y = 0
      while(y < h){
        //recursively update area
        if(cond(field(x)(y))){
          buf += replaceFieldCall(x,y)
        }
        y +=1
      }
      x += 1
    }
    buf
  }
}
