
object Solution {

    case class Point(var x:Int,var y:Int){
    }
    
    def ccw (p1:Point, p2:Point,p3:Point):Int = (p2.x-p1.x)*(p3.y-p1.y)-(p2.y-p1.y)*(p3.x-p1.x)
    
    def delta(p1:Point,p2:Point) = Point(p2.x-p1.x,p2.y-p1.y) 
        
    def LRPoint(ps:List[Point]):Point = {
        return ps.reduce((a:Point,b:Point) => {
        val d = delta(a,b)
        if (d.y > 0 || (d.y == 0 && d.x <= 0) ) a else b 
       })
     }
                             
    def parsePoint(str:String):Point = {
        val coords = str.split(" ")
        return Point(coords(0).toInt,coords(1).toInt)
    }
    
    def compareDelta(p:Int,q:Int):Int = {
        if (p >= 0 && q < 0) -1
        else if (q >= 0 && p < 0) +1
        else 0
    }
    
    def polarOrderSorted(ps:List[Point],lr:Point): List[Point] = {
        return ps.sortWith((p,q)=> {
             val pd = delta(lr,p)
             val qd = delta(lr,q)
             val cy = compareDelta(pd.y,qd.y)
            
             if (cy != 0) {cy > 0}
             else if (pd.y==0 && qd.y == 0) {compareDelta(pd.x,qd.x) > 0}
             else  {-ccw(lr,p,q) > 0 }
        }).reverse
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
          (sps.tail).foldLeft(List(lr,sps.head))(verifyPoint) 
    }
    
    def main(args: Array[String]) {
        val input : List[String] = io.Source.stdin.getLines().to[List]
        val lines = input.tail

        if (lines.length <= 1) {
           println(0)
        }
        else {
            val points = lines.map(parsePoint)
            val lr = LRPoint(points)
            // println("Lowest Right Point",lr.x,lr.y)
            val sPoints = polarOrderSorted(points.filter(_!=lr),lr)
            // printPoints(sPoints)
            val fPoints = convexHullPoints(lr,sPoints)
            // println("Convex Hull")
            // printPoints(fPoints)
            // println("Perimeter")
            println(perimeter(lr +: fPoints))
        }
    }
}
