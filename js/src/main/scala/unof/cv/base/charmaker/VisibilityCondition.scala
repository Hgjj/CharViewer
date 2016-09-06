package unof.cv.base.charmaker

sealed trait VisibilityCondition{
  def key : String
  
}
case object AlwayVisible extends VisibilityCondition{
  val key = "always"
}
case object VisibleIfNoLink extends VisibilityCondition{
  val key = "nolink"
}
object LinkedVisibility {
  val key = "linkedto"
}
case class LinkedVisibility(partKey : Int) extends VisibilityCondition{
  def key = LinkedVisibility.key
}
object SliderVisibility{
 val key = "compareslider"
 def parseOpp(s : String):(Int,Int)=>Boolean = s match {
   case "<" => _<_
    case "<=" => _<=_
    case ">" => _>_
    case ">=" => _>=_
    case "==" => _==_
    case "!=" => _!=_
    case other => 
      throw new IllegalArgumentException("Cannot parse "+other+" as a boolean binary opperator")
 }
 def parseOpp(opp : (Int,Int)=>Boolean):String = 
   if(opp(1,1)){
     if(opp(2,1)) 
       ">="
     else if(opp(1,2))
       "<="
     else
       "=="
   }else if(!opp(2,1))
     "<"
   else if(!opp(1,2))
     ">"
   else
     "!="
}
case class SliderVisibility(slider : String, opp : (Int,Int)=>Boolean, value : Int) extends VisibilityCondition{
  def key = SliderVisibility.key
}
