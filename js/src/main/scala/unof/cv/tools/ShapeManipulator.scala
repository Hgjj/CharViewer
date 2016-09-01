package unof.cv.tools

import unof.cv.base.charmaker.CMShape
import unof.cv.base.DrawingContext
import unof.cv.base.charmaker.DrawCommand
import unof.cv.base.Transforme
import unof.cv.base.Algebra._
import unof.cv.base.charmaker.CMShape
import unof.cv.base.charmaker.MoveTo
import unof.cv.base.charmaker.CurveTo
import unof.cv.base.charmaker.DrawCommand
import unof.cv.base.charmaker.MoveTo
import unof.cv.base.charmaker.CurveTo
import unof.cv.base.charmaker.DrawCommand
import unof.cv.base.charmaker.MoveTo
import unof.cv.base.charmaker.MoveTo
import unof.cv.base.charmaker.DrawCommand
import unof.cv.base.charmaker.CurveTo
import unof.cv.base.charmaker.MoveTo
import unof.cv.base.charmaker.MoveTo
import unof.cv.base.charmaker.CurveTo
import unof.cv.base.charmaker.CurveTo
import unof.cv.base.charmaker.MoveTo
import unof.cv.base.charmaker.MoveTo
import unof.cv.base.charmaker.CurveTo
import unof.cv.base.charmaker.MoveTo
import unof.cv.base.charmaker.CurveTo
import unof.cv.base.charmaker.CurveTo
import unof.cv.base.charmaker.MoveTo
import unof.cv.base.charmaker.CurveTo
import unof.cv.base.charmaker.CMShape
import unof.cv.base.charmaker.CMShape

object ShapeManipulator {
  def drawShapeHandles(shape: CMShape, transforms: Seq[Transforme], context: DrawingContext, settings: CvSetting) = {
    val ctx = context.ctx
    val handleSize = settings.shapeHandleSize.doubleValue()
    val curves = shape.commands


    ctx.setTransform(1, 0, 0, 1, 0, 0)
  
    val t = reduceTransforms(shape, transforms)

    curves.foldLeft((0.0,0.0)) {
      (prev, curve) =>
        drawCurve(curve, prev, t)
    }
    def drawBoundingPoint(pos: Vec) = {
      ctx.beginPath();
      ctx.arc(pos.x, pos.y, handleSize / 2, 0, 2 * math.Pi, false);
      ctx.fillStyle = "white"
      ctx.globalAlpha = 0.7
      ctx.fill();
      ctx.lineWidth = 2;
      ctx.globalAlpha = 1
      ctx.strokeStyle = "black";
      ctx.closePath()
      ctx.stroke();
    }
    def drawControlHandle(boundingPoint: Vec, handlePos: Vec) = {
      ctx.beginPath()
      ctx.moveTo(boundingPoint.x, boundingPoint.y)
      ctx.lineTo(handlePos.x, handlePos.y);
      ctx.strokeStyle = "black";
      ctx.lineWidth = 2
      ctx.stroke()
      ctx.strokeStyle = "white";
      ctx.lineWidth = 1
      ctx.closePath()
      ctx.stroke();

      ctx.fillStyle = "white"
      ctx.globalAlpha = 0.7
      val handleCenter = handlePos - (handleSize, handleSize) / 2
      ctx.fillRect(handleCenter.x, handleCenter.y, handleSize, handleSize)
      ctx.lineWidth = 2;
      ctx.globalAlpha = 1
      ctx.strokeStyle = "black";
      ctx.strokeRect(handleCenter.x, handleCenter.y, handleSize, handleSize)

    }

    def drawCurve(curve: DrawCommand, lastPoint: Vec, t: Transforme) = {
      
      curve match {
        case mt : MoveTo =>
          val to = t * mt.pos
          drawBoundingPoint(to)
          to
        case curve : CurveTo =>
          val cp1 = t * curve.cp1
          val cp2 = t * curve.cp2
          val end = t * curve.end
          drawControlHandle(lastPoint, cp1)
          drawControlHandle(end, cp2)
          drawBoundingPoint(end)
          end
          
      }

     
    }

  }
  private def reduceTransforms(s: CMShape, transforms: Seq[Transforme]) = {
    (transforms :+ s.transform).reduce(_ * _)
  }
  def click(
      mousePos: Vec,
      candidate: CMShape,
      transforms: Seq[Transforme],
      settings: CvSetting
  ): Option[(Int, Int)] = {
    val t = reduceTransforms(candidate, transforms)
    val hHandleSize = settings.shapeHandleSize.intValue() / 2
    val squareHandleRadius = hHandleSize * hHandleSize
    def inCircleBounds(circleCenter: Vec) = {
      (mousePos <<->> circleCenter) < squareHandleRadius
    }
    def inSquareBound(squareCenter: Vec) = {
      val dif = (mousePos - squareCenter).abs
      val hDim: Vec = (hHandleSize, hHandleSize)
      dif < hDim
    }

    candidate.commands.zipWithIndex.flatMap {
      case (mt : MoveTo,i) =>
        Seq((i, 0, mt.pos))
      case (ct : CurveTo,i) =>
        Seq((i, 0, ct.cp1), (i, 1, ct.cp2),(i, 2, ct.end))
    }
      .find {
        case (index, inPos, point) =>
          if (inPos == 1 || inPos == 2) {
            inSquareBound(t * point)
          } else {
            inCircleBounds(t * point)
          }
      }.map(t => (t._1, t._2))
    
  }
 
  def move(mousePos: Vec, movedPoint: (Int,Int), pointOwner: CMShape, callback: CallbackCenter, invertScreenMatrix: Transforme) = {
    val localMousePos = invertScreenMatrix * mousePos
    
    callback.onShapeManipulated(localMousePos,movedPoint._1,movedPoint._2)
    
  }
  def addCommande(targetShape : CMShape,
      selectedCommand : Int,
      commandPos : Vec
  ):(CMShape,Int) = {
    val commands = targetShape.commands
    def curveTo(from :Int)= new CurveTo(
      commands(from).last,
      commandPos,
      commandPos)
    val newDeltas = targetShape.deltas.map {
      t => (t._1,t._2.map(tt=> (tt._1,addCommande(tt._2, selectedCommand, commandPos)._1)))
    }
    val shape = targetShape.setDeltas(newDeltas)
    if(selectedCommand < 0 || commands.isEmpty)
      (shape.setDrawCommands(commands :+ new MoveTo(commandPos)),commands.size)
    else if(selectedCommand == commands.size-1)
      (shape.setDrawCommands(commands :+ curveTo(selectedCommand)),commands.size)
    else {
      targetShape.commands(selectedCommand) match {
        case mt : MoveTo =>
          val newComands = (commands.take(selectedCommand) :+
            new MoveTo(commandPos) :+
            new CurveTo(commandPos,mt.pos,mt.pos))++ commands.drop(selectedCommand+1)
          (shape.setDrawCommands(newComands),selectedCommand)
        case ct : CurveTo =>
          commands(selectedCommand+1) match {
            case _ :CurveTo =>
             ( shape.setDrawCommands(commands :+ new MoveTo(commandPos)),commands.size)
            case mt : MoveTo =>
              val newComands = (commands.take(selectedCommand+1):+
                new CurveTo(ct.end,commandPos,commandPos))++
                commands.drop(selectedCommand+1)
                (shape.setDrawCommands(newComands),selectedCommand+1)
          }
          
          
          
        
      }
      
    }
  }
  def removeCommande(targetShape : CMShape, commandIndex : Int):CMShape = {
    val oldCommands = targetShape.commands
    val newCommands = {
      if(commandIndex == oldCommands.size -1)
        oldCommands.dropRight(1)
      else oldCommands(commandIndex) match {
        case mt : MoveTo =>
          val next = oldCommands(commandIndex+1) match {
            case ct : CurveTo => new MoveTo(ct.end)
            case other : DrawCommand => other
          }
          (oldCommands.take(commandIndex) :+ next)++oldCommands.drop(commandIndex+2)
        case other => 
          oldCommands.take(commandIndex) ++ oldCommands.drop(commandIndex+1)
      }
    }
    val newDeltas = targetShape.deltas.map {
      t => (t._1,t._2.map(tt=> (tt._1,removeCommande(tt._2, commandIndex))))
    }
    targetShape.setDrawCommands(newCommands).setDeltas(newDeltas)
  }
}