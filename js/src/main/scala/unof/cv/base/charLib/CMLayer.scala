package unof.cv.base.charLib

import unof.cv.utils.Transforme

object CMLayer {
  private var id = 0;
  def newId = {
    val usedId = id
    id += 1
    usedId
  }
  
  def sorting(l : CMLayer) = l match{
    case im : CMImage => (0,im.ref)
    case other => (1,""+other.hashCode())
  }
}
trait CMLayer {
  def name : String
  def transform: Transforme
  def boundColors: Seq[String]
  def z: Float
  def displayCondition: VisibilityCondition
  
  val id = CMLayer.newId
  
  def setTransform(newTransform: Transforme): CMLayer
  def setColorBonds(newColors: Seq[String]): CMLayer
  def setZ(newZ: Float): CMLayer
  def setCondition(newCondition: VisibilityCondition): CMLayer
  def setName (newName : String):CMLayer
  def changeId : CMLayer
  
}