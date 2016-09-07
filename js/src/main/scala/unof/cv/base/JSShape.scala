package unof.cv.base
import scala.scalajs.js

@js.native
trait JSShape extends js.Object {
  def points : js.Array[js.Array[JSVec]]
  def transform : js.Array[Number]
  def colorVariables : js.Array[JSDynamColor]
  def name : String
  
  def z_layer : Number
  def condition : js.Array[String]
  def lineWidth : Number
  def showSurface : Boolean
  def lineJoin : String
  def closed : Boolean
  def deltaLink : JSDelta
}
@js.native
trait JSVec extends js.Object {
  def x : Number
  def y : Number
}