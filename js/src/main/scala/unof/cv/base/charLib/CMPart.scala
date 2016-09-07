package unof.cv.base.charLib

import unof.cv.utils.Transforme

object CMPart {
  private var link = 0;
  def newLinkKey = {
    val usedLinkKey = link
    link += 1
    usedLinkKey
  }
}
class CMPart(
    val partName: String,
    val images: Seq[CMImage],
    val shapes: Seq[CMShape],
    val partTransform: Transforme,
    val partZ: Float,
    val linkKey: Int) {
  if (partName.isEmpty())
    throw new IllegalArgumentException("Parts must have a name")
  def setName(name: String) = new CMPart(name, images, shapes, partTransform, partZ, linkKey)
  def setImages(newImages: Seq[CMImage]) = new CMPart(partName, newImages, shapes, partTransform, partZ, linkKey)
  def setShapes(newShapes: Seq[CMShape]) = new CMPart(partName, images, newShapes, partTransform, partZ, linkKey)
  def setTransform(transform: Transforme) = new CMPart(partName, images, shapes, transform, partZ, linkKey)
  def setZ(z: Float) = new CMPart(partName, images, shapes, partTransform, z, linkKey)
  def removeLink = new CMPart(partName, images, shapes, partTransform, partZ, CMPart.newLinkKey)
  def components = images ++ shapes
}