
object Solution {

    case class Point(var x:Int,var y:Int){
      
    }
    
    def ccw (p1:Point, p2:Point,p3:Point):Int = (p2.x-p1.x)*(p3.y-p1.y)-(p2.y-p1.y)*(p3.x-p1.x)
          
    def LRPoint(ps:List[Point]):Point = {
        return ps.reduceLeft((a:Point,b:Point) => {
        if (a.y < b.y){
            return a
        }
        else if (b.y < a.y){
            return b
        }
        else if (a.x > b.y){
            return a
        }
        else {
            return b
        }
     })
                             }
                             
    def parsePoint(str:String):Point = {
        val coords = str.split(" ")
        return Point(coords(0).toInt,coords(1).toInt)
    }
    
    def sortPoints(ps:List[Point],lr:Point): List[Point] = {
        return ps.sortBy(p=> {
            (p.y - lr.y)/(1.0*( p.x - lr.x) )
            
        })
    }
    
    def printPoints(ps:List[Point]){
        for (p <- ps){
            println(p.x,p.y)
        }
    }
    
    def distance(p1:Point,p2:Point):Double = scala.math.sqrt(
        scala.math.pow(p2.x-p1.x,2) +
        scala.math.pow(p2.y-p1.y,2)
    )
        
    def perimeter(ps:List[Point]):Double = {
        val start = ps.head
        val route = ps :+ start
        return route.foldLeft((start,0.0))((a,p)=>{
            val prev = a._1
            var total = a._2
            (p,total+distance(prev,p))
        })._2
        
    }
    
    def verifyPoint(a:List[Point],p:Point):List[Point]={
         val start = a.init.last
         val pivot = a.last
         val pangle = ccw(start,pivot,p)
        
         if (a.length <= 2 || pangle > 0) {
            return a :+ p
         }
         else {
            return verifyPoint(a.init,p)
         }
    }
    
    def convexHullPoints(lr:Point,sps:List[Point]):List[Point]= {
          sps.tail.foldLeft(List(lr,sps.head))(verifyPoint) 
    }
    
    def main(args: Array[String]) {
        val input : List[String] = io.Source.stdin.getLines().to[List]
        //val input : List[String] =  List("3","0 0","3 0","3 4")
        val lines = input.tail

        if (lines.length <= 1) {
           println(0)
        }
        else {
            val points = lines.map(parsePoint)
            val lr = LRPoint(points)
            val sPoints = sortPoints(points.filter(_!=lr),lr)
            val fPoints = convexHullPoints(lr,sPoints)
            println(perimeter(lr +: fPoints))
        }
    }
}
