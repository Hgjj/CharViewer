package unof.cv.base.charmaker

import unof.cv.base.Transforme

class CMImage(
    val ref: String,
    val transform: Transforme,
    val boundColor: String,
    val z: Float,
    val displayCondition: VisibilityCondition,
    val name  :String) extends CMLayer {
 
  def boundColors = Seq(boundColor)
  def setRef(newRef: String) =
    new CMImage(
      newRef,
      transform,
      boundColor,
      z,
      displayCondition,
      name)
  def setTransform(newTransform: Transforme) =
    new CMImage(
      ref,
      newTransform,
      boundColor,
      z,
      displayCondition,
      name)
  def setColorBond(newColor: String) =
    new CMImage(
      ref,
      transform,
      newColor,
      z,
      displayCondition,
      name)
  def setColorBonds(newColors: Seq[String]) =
    new CMImage(
      ref,
      transform,
      if (newColors.isEmpty) "None" else newColors(0),
      z,
      displayCondition,
      name)
  def setZ(newZ: Float) =
    new CMImage(
      ref,
      transform,
      boundColor,
      newZ,
      displayCondition,
      name)
  def setCondition(newCondition: VisibilityCondition) =
    new CMImage(
      ref,
      transform,
      boundColor,
      z,
      newCondition,
      name)
  def setName(newName: String) =
    new CMImage(
      ref,
      transform,
      boundColor,
      z,
      displayCondition,
      newName)
  def changeId =
    new CMImage(
      ref,
      transform,
      boundColor,
      z,
      displayCondition,
      name)

  override def toString = ref
}