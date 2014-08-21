val n = 9
val pairs = "12342321323".groupBy(identity).map{case (c,str)=>(c,str.length)}
val res@min::max::Nil = pairs.minBy(_._2)::pairs.maxBy(_._2)::Nil
res map { case (c,n) => c + " " + n } foreach println
def pick(p:Double):Int = {
  val r = Math.random()
  if(0 <=  r && r < p) 1 else 0
}
val t = (100,100)
val res2 = t match {
  case (a,b) => a + b
}
Iterator.continually(pick(0.02)).scanLeft(0){
  case (a,b) => a + b
}.zipWithIndex.drop(150).take(100).foreach(println)