package archives

import archives.H20W.E.{Door, Move, Lamp}
import scala.collection.mutable.ArrayBuffer
import scala.collection.GenTraversableOnce
import archives.H25S.M

/**
 * できなかった
 * と思いきやあと60分あるぞ、やってみなければ
 * @author glyph
 */
object H20W {
  def q1_1() = ??? //3 から　1まで7秒、乗り込み7秒、1から7で10秒、降りるのに１秒より25秒
  def q1_2() = ??? //乗り込みまで同じ、1から６で９秒 ，乗り込み７秒、６から７で４秒、降りるのに１秒 => 35秒
  def q3 = ???

  class EM(N:Int){
    val elevators = Array.fill(N)(new E)
    def request(from:Int,dir:Int){}
  }
  object E{
    trait LampState
    object Lamp{
      val Off = 0
      val Up = 1
      val Down = -1
    }
    object Move{
      val Stopped = 0
      val Starting = 1
      val FastMove = 2
      val Breaking = 3
    }
    object Door{
      val Closed = 0
      val Opening = 1
      val Opened = 2
      val Closing = 3
    }
  }
  //1フロアは12の距離
  class Door{
    var timer = -1
    val L = 7
    def open(){
      timer = 0
    }
    def isOpened = timer != -1
    def step(){
      if(timer != -1) {
        timer += 1
      }
      if(timer == 7) {
        timer = -1
      }
    }
  }
  class E{
    var pos = 0
    var speed = 0
    var lamp = Lamp.Off
    var moving = false
    val door = new Door
    val requests = ArrayBuffer[(Int,Int)]()
    val targets = ArrayBuffer[Int]()
    val plans = ArrayBuffer[Int]()
    def receive(from:Int,dir:Int){
      requests += (from->dir)
    }
    def uppers = requests.filter{
      case (f,d) => d > 0
    }
    def downers = requests.filter{
      case (f,d) => d < 0
    }
    def step(){
      //state
      //stopped (idle)
      //moving
      //stopped (wait door)
      if(moving){
        pos += speed
        if(pos == plans.head){ //arrived
          door.open()
          requests -= requests.find{
            case (f,d) => f == pos
          }.get
        }
        moving = false
        speed = 0
      }else{
       if(door.isOpened){
         //wait
       } else{
         //get destination
         plan()
         val next = plans.head

       }
      }
    }
    implicit def tuple2Seq[T](t:(T,T)):Seq[T] = Seq(t._1,t._2)
    def plan(){
      //check lamp
      if(lamp == Lamp.Off){
        val (from,dir) = requests.sortBy{
          case (f,d) => Math.abs(pos - f)
        }.head
        lamp = dir
      }
      val plan = if(lamp < 0){
        val (first,second) = uppers.partition{
          case(f,d) => pos < f && f <= 10
        }
        (first.map(_._1)++targets).sorted ++
        downers.map(_._1).sorted.reverse ++
        second.map(_._1).sorted
      } else{
        val (first,second) = downers.partition{
          case(f,d) => 1 <= f && f < pos
        }
        (first.map(_._1)++targets).sorted.reverse ++
          downers.map(_._1).sorted ++
          second.map(_._1).sorted.reverse
      }
      plans.clear()
      plans ++= plan
    }
  }
}
