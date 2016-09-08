package unof.cv.base

import org.scalajs.dom.raw.HTMLImageElement
import unof.cv.utils.Transforme
import unof.cv.base.charLib.ImageRef

class DiffImage(
  val image: Option[HTMLImageElement],
  val transform: Transforme,
  val color: (Float, Float, Float),
  val z: Float,
  val locationId: Int,
  val dAlpha : Float)