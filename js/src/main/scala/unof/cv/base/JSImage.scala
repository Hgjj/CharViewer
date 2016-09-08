package unof.cv.base
import scala.scalajs.js

@js.native
trait JSImage extends js.Object {
  def imageRef : String
  def transform : js.Array[Number]
  def colorVariable : String
  def z_layer : Number
  def condition : js.Array[String]
  def name : String
  def deltaLink : JSDelta
}