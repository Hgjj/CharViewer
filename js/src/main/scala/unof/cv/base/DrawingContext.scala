package unof.cv.base
import org.scalajs.dom.raw.HTMLCanvasElement
import org.scalajs.dom.raw.CanvasRenderingContext2D
import scala.scalajs.js.Dynamic.{ global => g }
import scala.scalajs.js.Any.fromString
import scala.scalajs.js.Dynamic.{global => g}
import unof.cv.base.Algebra._

class DrawingContext (
   /**
   * @return The id of the canvas on which this object draw.
   */
  val canvasName: String,
  dimension : Vec = (0,0)
){
  
  /**
   * @return The dimensions in px of the canvas on which this object draw.
   */
  def dimensions: (Double,Double) = (canvasElem.width, canvasElem.height)
  val canvasOrig = if(canvasName.isEmpty())
    g.document.createElement("canvas")
  else
    g.document.getElementById(canvasName)
  val canvasElem = canvasOrig.asInstanceOf[HTMLCanvasElement]
  
  if(canvasName.isEmpty()){
    canvasElem.width = dimension.x
    canvasElem.height = dimension.y
  }
  /**
   *  The CanvasRenderingContext2D used by this object.
   */
  val ctx = canvasElem.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
}