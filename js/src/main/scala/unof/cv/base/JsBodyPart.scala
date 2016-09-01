package unof.cv.base
import scala.scalajs.js

@js.native
trait JsBodyPart extends js.Object{
  def partName : String
  def category : String
  def partZ : Number
  def partTransform : js.Array[Number]
  def components : js.Array[js.Dynamic]
}