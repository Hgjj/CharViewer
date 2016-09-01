package unof.cv.base
import scala.scalajs.js

@js.native
trait JSShape extends js.Object {
  def points : js.Array[js.Array[JSVec]]
  def transform : js.Array[Number]
  def colorVariables : js.Array[String]
  
  def z_layer : Number
  def condition : js.Array[String]
  def lineWidth : Number
  def showSurface : Boolean
  def lineJoin : String
  def closed : Boolean
  def deltas : js.Array[js.Array[JSDelta]]
  def linkedSlider : js.Array[String]
}
@js.native
trait JSVec extends js.Object {
  def x : Number
  def y : Number
}