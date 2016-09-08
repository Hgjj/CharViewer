package unof.cv.base.charLib

import unof.cv.utils.Transforme

class CMImage(
    val ref: String,
    val transform: Transforme,
    val boundColor: String,
    val z: Float,
    val displayCondition: VisibilityCondition,
    val deltaLink  :DeltaLink,
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
      name)
  def setTransform(newTransform: Transforme) =
    new CMImage(
      ref,
      newTransform,
      boundColor,
      z,
      displayCondition,
      deltaLink,
      name)
  def setColorBond(newColor: String) =
    new CMImage(
      ref,
      transform,
      newColor,
      z,
      displayCondition,
      deltaLink,
      name)
  def setColorBonds(newColors: Seq[String]) =
    new CMImage(
      ref,
      transform,
      if (newColors.isEmpty) "None" else newColors(0),
      z,
      displayCondition,
      deltaLink,
      name)
  def setZ(newZ: Float) =
    new CMImage(
      ref,
      transform,
      boundColor,
      newZ,
      displayCondition,
      deltaLink,
      name)
  def setCondition(newCondition: VisibilityCondition) =
    new CMImage(
      ref,
      transform,
      boundColor,
      z,
      newCondition,
      deltaLink,
      name)
  def setName(newName: String) =
    new CMImage(
      ref,
      transform,
      boundColor,
      z,
      displayCondition,
      deltaLink,
      newName)
  
  def setDeltaLink(newDelta : DeltaLink) = 
    new CMImage(
      ref,
      transform,
      boundColor,
      z,
      displayCondition,
      newDelta,
      name)
  def changeId =
    new CMImage(
      ref,
      transform,
      boundColor,
      z,
      displayCondition,
      deltaLink,
      name)

  override def toString = ref
}