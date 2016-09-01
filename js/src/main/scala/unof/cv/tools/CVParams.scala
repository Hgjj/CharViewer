package unof.cv.tools
import scala.scalajs.js
import unof.cv.base.JsBodyPart

@js.native
trait CVParams extends js.Object{
  def bodyParts : js.Array[JsBodyPart]
  def colors : js.Array[String]
  def choices : js.Array[Number]
  def selected : js.Array[Number]
  def sliders : js.Array[Number]
  def date : Number
}