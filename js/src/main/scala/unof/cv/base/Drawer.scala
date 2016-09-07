package unof.cv.base

import unof.cv.base.charLib.DrawCommand
import unof.cv.utils.Algebra._
import org.scalajs.dom.raw.CanvasRenderingContext2D
import org.scalajs.dom.raw.{ HTMLImageElement => Image }
import org.scalajs.dom.raw.HTMLCanvasElement
import scala.scalajs.js.Dynamic
import unof.cv.base.charLib.DrawCommand
import unof.cv.base.charLib.MoveTo
import unof.cv.base.charLib.CurveTo
import unof.cv.utils.Transforme
import unof.cv.utils.Algebra

trait Drawer {

  private val layerCanvas = Dynamic.global.document
    .createElement("canvas").asInstanceOf[HTMLCanvasElement];
  private val layerContext = layerCanvas.getContext("2d")
    .asInstanceOf[CanvasRenderingContext2D];

  def drawShape(
    controlPoints: Seq[DrawCommand],
    surfaceColor: String,
    surfaceAlpha: Float,
    showSurcface: Boolean,
    lineColor: String,
    lineAlpha: Float,
    lineWidth: Int,
    lineJoin : String,
    transforms: Seq[Transforme],
    isClosed : Boolean,
    context: DrawingContext
    ) = if (controlPoints.size > 0) {

    val ctx = context.ctx;
    
    ctx.beginPath()
    
    ctx.setTransform(1, 0, 0, 1, 0, 0)
   transforms.foreach {
      t =>

        ctx.transform(t.m11, t.m12, t.m21, t.m22, t.dx, t.dy)
    }
    controlPoints.foreach {
      case moveTo : MoveTo =>
        val start = moveTo.pos
        ctx.moveTo(start._1, start._2)
      case curveTo : CurveTo =>
        val ctr1 = curveTo.cp1
        val ctr2 = curveTo.cp2
        val end = curveTo.end
        ctx.bezierCurveTo(ctr1._1, ctr1._2, ctr2._1, ctr2._2, end._1, end._2)
      
    }
    if(isClosed){
      ctx.closePath()
    }
      
    ctx.setTransform(1, 0, 0, 1, 0, 0)
    if (showSurcface) {
      ctx.fillStyle = surfaceColor
      ctx.globalAlpha = surfaceAlpha;
      ctx.fill()
      ctx.globalAlpha = 1
    }
    if (lineWidth > 0) {
      ctx.lineJoin = lineJoin
      ctx.strokeStyle = lineColor
      ctx.lineWidth = lineWidth
      ctx.globalAlpha = lineAlpha;
      ctx.stroke()
      ctx.globalAlpha = 1
    }
    if(!isClosed){
      ctx.closePath()
    }
    
  }
  def drawImage(img: Image, color: String, ts: Seq[Transforme], context: DrawingContext) = {
    val ctx = context.ctx;
    ctx.setTransform(1, 0, 0, 1, 0, 0)
    
    ts.foreach {
      t =>
        ctx.transform(t.m11, t.m12, t.m21, t.m22, t.dx, t.dy)
    }

    if (color != "white" && color.tail.exists { _.toLower != 'f' }) {

      layerCanvas.width = img.width;
      layerCanvas.height = img.height;
      layerContext.setTransform(1, 0, 0, 1, 0, 0);
      layerContext.clearRect(0, 0, img.width, img.height)
      layerContext.globalCompositeOperation = "copy"
      layerContext.drawImage(img, 0, 0)
      layerContext.globalCompositeOperation = "multiply"
      layerContext.fillStyle = color
      layerContext.fillRect(0, 0, img.width, img.height)
      layerContext.globalCompositeOperation = "destination-in"
      layerContext.drawImage(img, 0, 0)
      val ctxPrevCompo = ctx.globalCompositeOperation
      ctx.globalCompositeOperation = "source-over";
      ctx.drawImage(layerCanvas, 0, 0);
    } else {
      ctx.drawImage(img, 0, 0);
    }

  }
}