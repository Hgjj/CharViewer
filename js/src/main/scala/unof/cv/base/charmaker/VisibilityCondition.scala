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
