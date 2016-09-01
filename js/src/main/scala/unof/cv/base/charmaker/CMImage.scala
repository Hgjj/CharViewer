package unof.cv.base.charmaker

import unof.cv.base.Transforme


class CMImage(
    val ref: String,
    val transform: Transforme,
    val boundColor: String,
    val z: Float,
    val displayCondition : VisibilityCondition) extends CMLayer {
  def boundColors = Seq(boundColor)
  def setRef(newRef : String) = 
    new CMImage (
        newRef,
        transform,
        boundColor,
        z,
        displayCondition
    )
  def setTransform(newTransform: Transforme) = 
    new CMImage (
        ref,
        newTransform,
        boundColor,
        z,
        displayCondition
    )
   def setColorBond(newColor : String) = 
    new CMImage (
        ref,
        transform,
        newColor,
        z,
        displayCondition
    )
  def setColorBonds(newColors : Seq[String]) = 
    new CMImage (
        ref,
        transform,
        if(newColors.isEmpty) "None" else newColors(0),
        z,
        displayCondition
    )
   def setZ(newZ : Float) = 
    new CMImage (
        ref,
        transform,
        boundColor,
        newZ,
        displayCondition
    )
   def setCondition(newCondition : VisibilityCondition) = 
    new CMImage (
        ref,
        transform,
        boundColor,
        z,
        newCondition
    )
 
  def changeId= 
    new CMImage (
        ref,
        transform,
        boundColor,
        z,
        displayCondition
    )
  
  override def toString = ref
}