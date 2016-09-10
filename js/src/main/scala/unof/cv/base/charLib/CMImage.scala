package unof.cv.base.charLib

import unof.cv.utils.Transforme

object CMImage {
  def apply(ref : String):CMImage = {
    new CMImage(ref,Transforme(),"None",0,AlwayVisible,DeltaLink(),1,ref)
  }
  def apply():CMImage = {
    apply("None")
  }
}
class CMImage(
    val ref: String,
    val transform: Transforme,
    val boundColor: String,
    val z: Float,
    val displayCondition: VisibilityCondition,
    val deltaLink  :DeltaLink,
    val alpha : Float,
    val name  :String) extends CMLayer {
 
  def boundColors = Seq(boundColor)
  def setRef(newRef: String) =
    new CMImage(
      newRef,
      transform,
      boundColor,
      z,
      displayCondition,
      deltaLink,
      alpha,
      name)
  def setTransform(newTransform: Transforme) =
    new CMImage(
      ref,
      newTransform,
      boundColor,
      z,
      displayCondition,
      deltaLink,
      alpha,
      name)
  def setColorBond(newColor: String) =
    new CMImage(
      ref,
      transform,
      newColor,
      z,
      displayCondition,
      deltaLink,
      alpha,
      name)
  def setColorBonds(newColors: Seq[String]) =
    new CMImage(
      ref,
      transform,
      if (newColors.isEmpty) "None" else newColors(0),
      z,
      displayCondition,
      deltaLink,
      alpha,
      name)
  def setZ(newZ: Float) =
    new CMImage(
      ref,
      transform,
      boundColor,
      newZ,
      displayCondition,
      deltaLink,
      alpha,
      name)
  def setCondition(newCondition: VisibilityCondition) =
    new CMImage(
      ref,
      transform,
      boundColor,
      z,
      newCondition,
      deltaLink,
      alpha,
      name)
  def setName(newName: String) =
    new CMImage(
      ref,
      transform,
      boundColor,
      z,
      displayCondition,
      deltaLink,
      alpha,
      newName)
  
  def setDeltaLink(newDelta : DeltaLink) = 
    new CMImage(
      ref,
      transform,
      boundColor,
      z,
      displayCondition,
      newDelta,
      alpha,
      name)
  def setAlpha(newAlpha : Float) = 
    new CMImage(
      ref,
      transform,
      boundColor,
      z,
      displayCondition,
      deltaLink,
      newAlpha,
      name)
  def changeId =
    new CMImage(
      ref,
      transform,
      boundColor,
      z,
      displayCondition,
      deltaLink,
      alpha,
      name)

  override def toString = ref
}