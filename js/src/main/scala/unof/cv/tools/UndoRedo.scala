package unof.cv.tools

import scala.scalajs.js.Date


class UndoRedo(val minimalDifCooldown : Double, val maxUndoLength : Int, val bunchingTime : Double) {
  private var undoPile : Seq[Diff] = Nil
  private var redoPile : Seq[Diff] = Nil
  
  def makeDif(oldStat : Any, newStat : Any) = if(oldStat != newStat){
    redoPile = Nil
    val now =  Date.now()
    undoPile match {
      case Nil =>
        undoPile = new Diff(oldStat,newStat,now) ::Nil
      case lastDif :: tail =>
        if(lastDif.newStat == oldStat && now -lastDif.date <= minimalDifCooldown){
          undoPile = new Diff(lastDif.oldStat,newStat,now) :: tail
        }else{
          undoPile = new Diff(oldStat,newStat,now) +: undoPile
        }
        
    }
    if(undoPile.size > maxUndoLength) {
      undoPile = undoPile.dropRight(1)
    }
  }
  def  reset = {
    undoPile = Nil
    redoPile = Nil
  }
  def undo : Option[Any] = undoPile match {
    case Nil => None
    case seq =>
      val bunch = if(bunchingTime < 0)
        Seq(seq.head)
      else {
        seq.takeWhile { x => seq.head.date - x.date <= bunchingTime }
          
      }
      undoPile = undoPile.drop(bunch.length);
      redoPile = bunch.reverse ++ redoPile
      
      bunch match {
        case head :: Nil => Some(head.oldStat)
        case seq => Some(BunchedDiffs(seq.map(_.oldStat)))
      }
  }
  def redo : Option[Any] = redoPile match{
    case Nil => None
    case seq =>
      val bunch = if(bunchingTime < 0)
        Seq(seq.head)
      else {
        seq
          .takeWhile { x => x.date - seq.head.date <= bunchingTime }
          
      }
      redoPile = redoPile.drop(bunch.length);
      
      undoPile = bunch.reverse ++ undoPile
      bunch match {
        case head :: Nil => Some(head.newStat)
        case seq => Some(BunchedDiffs(seq.map(_.newStat)))
      }
  }
  
  def topDiffDate = {
    if(undoPile.isEmpty){
      Double.MaxValue
    }
    else
      undoPile.head.date
  }
  def canUndo = !undoPile.isEmpty
  def canRedo = !redoPile.isEmpty
}
case class BunchedDiffs(difs : Seq[Any])
class Diff (val oldStat : Any, val newStat : Any, val date : Double){
  override def toString = (oldStat,newStat,date).toString()
}