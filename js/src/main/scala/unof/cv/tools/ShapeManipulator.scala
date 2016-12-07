package unof.cv.tools

import unof.cv.base.charLib.CMShape
import unof.cv.base.DrawingContext
import unof.cv.base.charLib.DrawCommand
import unof.cv.utils.Transforme
import unof.cv.utils.Algebra._
import unof.cv.base.charLib.CMShape
import unof.cv.base.charLib.MoveTo
import unof.cv.base.charLib.CurveTo
import unof.cv.base.charLib.DrawCommand
import unof.cv.base.charLib.MoveTo
import unof.cv.base.charLib.CurveTo
import unof.cv.base.charLib.DrawCommand
import unof.cv.base.charLib.MoveTo
import unof.cv.base.charLib.MoveTo
import unof.cv.base.charLib.DrawCommand
import unof.cv.base.charLib.CurveTo
import unof.cv.base.charLib.MoveTo
import unof.cv.base.charLib.MoveTo
import unof.cv.base.charLib.CurveTo
import unof.cv.base.charLib.CurveTo
import unof.cv.base.charLib.MoveTo
import unof.cv.base.charLib.MoveTo
import unof.cv.base.charLib.CurveTo
import unof.cv.base.charLib.MoveTo
import unof.cv.base.charLib.CurveTo
import unof.cv.base.charLib.CurveTo
import unof.cv.base.charLib.MoveTo
import unof.cv.base.charLib.CurveTo
import unof.cv.base.charLib.CMShape
import unof.cv.base.charLib.CMShape
import unof.cv.base.charLib.CMPart
import unof.cv.base.charLib.CMShape
import unof.cv.base.charLib.CMShape
import unof.cv.base.charLib.MoveTo
import unof.cv.base.charLib.CurveTo
import unof.cv.base.charLib.MoveTo
import unof.cv.base.charLib.MoveTo
import unof.cv.base.charLib.CurveTo
import unof.cv.base.charLib.CurveTo
import unof.cv.base.charLib.DrawCommand
import unof.cv.base.charLib.CurveTo
import unof.cv.base.charLib.MoveTo

object ShapeManipulator {
  def drawShapeHandles(shape: CMShape, transforms: Seq[Transforme], context: DrawingContext, settings: CvSetting, selectedCommand: Int) = {
    val ctx = context.ctx
    val handleSize = settings.shapeHandleSize.doubleValue()
    val curves = shape.commands

    ctx.setTransform(1, 0, 0, 1, 0, 0)

    val t = reduceTransforms(shape, transforms)
    val center = t * (0, 0)

    curves.zipWithIndex.foldLeft((0.0, 0.0)) {
      (prev, curveindex) =>
        drawCurve(curveindex._1, prev, t, curveindex._2)
    }

    drawCenter(center)

    def drawCenter(pos: Vec) = {
      val hsHalf = handleSize / 2

      def drawReticula = {
        ctx.beginPath();
        ctx.arc(pos.x, pos.y, hsHalf, 0, 2 * math.Pi, false);
        ctx.closePath()
        ctx.stroke();
        var start: Vec = (hsHalf / 2, 0)
        var end: Vec = (hsHalf / 2 + hsHalf, 0)
        (1 to 4) foreach {
          i =>
            val s = pos + start
            val e = pos + end
            ctx.beginPath()
            ctx.moveTo(s.x, s.y)
            ctx.lineTo(e.x, e.y)
            ctx.closePath()
            ctx.stroke()
            start = start.halfPiRotate
            end = end.halfPiRotate
        }
      }

      ctx.strokeStyle = "black";
      ctx.lineWidth = 3
      drawReticula
      ctx.strokeStyle = "white";
      ctx.lineWidth = 1
      drawReticula

    }
    def drawBoundingPoint(pos: Vec, selected: Boolean) = {
      ctx.beginPath();
      ctx.arc(pos.x, pos.y, handleSize / 2, 0, 2 * math.Pi, false);
      ctx.fillStyle = "white"
      ctx.globalAlpha = 0.7
      if (!selected)
        ctx.fill();
      ctx.lineWidth = 2;
      ctx.globalAlpha = 1
      ctx.strokeStyle = "black";
      ctx.closePath()
      ctx.stroke();
      if (selected) {
        ctx.lineWidth = 1;
        ctx.globalAlpha = 1
        ctx.strokeStyle = "white";
        ctx.stroke();
      }

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

    def drawCurve(curve: DrawCommand, lastPoint: Vec, t: Transforme, index: Int) = {

      curve match {
        case mt: MoveTo =>
          val to = t * mt.pos
          drawBoundingPoint(to, selectedCommand < 0 || index == selectedCommand)
          to
        case curve: CurveTo =>
          val cp1 = t * curve.cp1
          val cp2 = t * curve.cp2
          val end = t * curve.end
          if (selectedCommand < 0 || index - 1 == selectedCommand)
            drawControlHandle(lastPoint, cp1)
          if (selectedCommand < 0 || index == selectedCommand)
            drawControlHandle(end, cp2)
          drawBoundingPoint(end, index == selectedCommand)

          end

      }

    }

  }
  private def reduceTransforms(s: CMShape, transforms: Seq[Transforme]) = {
    (transforms :+ s.transform).reduce(_ * _)
  }
  def click(
    mousePos: Vec,
    shape: CMShape,
    transforms: Seq[Transforme],
    settings: CvSetting,
    selectedIndex: Int): Option[(Int, Int)] = {

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
    val transform = reduceTransforms(shape, transforms)
    val center = transform * (0, 0)
    if (inCircleBounds(center))
      Some((-1, -1))
    else
      shape.commands.zipWithIndex.flatMap {
        case (mt: MoveTo, i) =>
          Seq((i, 0, mt.pos))
        case (ct: CurveTo, i) =>
          Seq((i, 1, ct.cp1), (i, 2, ct.cp2), (i, 0, ct.end))
      }
        .find {
          case (index, inPos, point) =>
            if (selectedIndex < 0 ||
              inPos == 0 || inPos == 2 && index == selectedIndex ||
              inPos == 1 && index - 1 == selectedIndex) {
              if (inPos > 0) {
                inSquareBound(transform * point)
              } else {
                inCircleBounds(transform * point)
              }
            } else false

        }
        .map(t => (t._1, t._2))

  }
  def projectOnCurve(target: Vec, squareMaging: Double, p0: Vec, p1: Vec, p2: Vec, p3: Vec) = {
    val split = splitBezierCurve(60, p0, p1, p2, p3, 0, 1)
    
    val  v = getCloseSegment(target, squareMaging * 4, split)
    .flatMap {
      case (((start, end), d)) =>
        val subsplit = splitBezierCurve(60, p0, p1, p2, p3, start, end)
        val vv = getCloseSegment(target, squareMaging, subsplit).map {
          case (((startT, endT), err)) =>
            (
              (startT + endT) / 2,
              target <<->> interpolateAt(((startT + endT) / 2), p0, p1, p2, p3))
        }
        vv.headOption
    }
    .sortBy(_._2)
    v.headOption
  }

  def projectOnShape(
    shape: CMShape,
    projected: Vec,
    transforms: Seq[Transforme],
    errorMaging: Float) = {
    val sqrMargin = errorMaging * errorMaging;
    val t = reduceTransforms(shape, transforms)
    val res = shape.commands.zipWithIndex.foldLeft(((0.0, 0.0), None: Option[(Double, Int)], Double.PositiveInfinity)) {
      case ((prev, best, dist), (mt: MoveTo, idx)) =>
        (t*mt.pos, best, dist)
      case ((prev, None, dist), (ct: CurveTo, idx)) =>
        val end = t * ct.end
        projectOnCurve(projected, errorMaging*errorMaging, prev, t * ct.cp1, t * ct.cp2, end) match {
          case None => (end, None, dist)
          case Some((t, error)) =>
            (end, Some((t, idx)), error)
        }

      case ((prev, Some((bestT, idxBt)), dist), (ct: CurveTo, idx)) =>
        val end = t * ct.end
        projectOnCurve(projected, errorMaging*errorMaging, prev, t * ct.cp1, t * ct.cp2, end) match {
          case None => (end, Some((bestT, idxBt)), dist)
          case Some((t, error)) =>
            if (error < dist)
              (end, Some((t, idx)), error)
            else
              (end, Some((bestT, idxBt)), dist)
        }
    }
    (res._2, res._3)
  }
  def addHandle(t: Double, start: Vec, curve: DrawCommand): Seq[CurveTo] = {
    val (p1, p2, p3) = curve match {
      case ct: CurveTo => (ct.cp1, ct.cp2, ct.end)
      case mt: MoveTo  => (start, mt.pos, mt.pos)
    }
    val p0_1 = start * (1 - t) + p1 * t
    val p1_2 = p1 * (1 - t) + p2 * t
    val p2_3 = p2 * (1 - t) + p3 * t

    val p01_12 = p0_1 * (1 - t) + p1_2 * t
    val p12_23 = p1_2 * (1 - t) + p2_3 * t

    val p0112_1223 = p01_12 * (1 - t) + p12_23 * t

    Seq(
      new CurveTo(p0_1, p01_12, p0112_1223),
      new CurveTo(p12_23, p2_3, p3))
  }
  def move(mousePos: Vec, movedPoint: (Int, Int), pointOwner: CMShape, callback: CallbackCenter, invertScreenMatrix: Transforme, invertPartMatrix: Transforme) = {
    val localMousePos = invertScreenMatrix * mousePos
    if (movedPoint._1 < 0) {
      callback.onShapeOriginChanged(invertPartMatrix * mousePos)
    } else
      callback.onShapeManipulated(localMousePos, movedPoint._1, movedPoint._2)

  }
  def addCommande(
    hostPart: CMPart,
    deltaGroup: Int,
    selectedCommand: Int,
    commandPos: Vec): (CMPart, Int) = {
    var soureNewCommandPos = -1
    val newShapes = hostPart.shapes.map {
      s =>
        if (s.deltaLink.key == deltaGroup) {
          val v = addCommande(s, selectedCommand, commandPos)
          if (s.deltaLink.isSource)
            soureNewCommandPos = v._2
          v._1
        } else s
    }
    (hostPart.setShapes(newShapes), soureNewCommandPos)
  }

  private def addCommande(
    shape: CMShape,
    selectedCommand: Int,
    commandPos: Vec): (CMShape, Int) = {
    val commands = shape.commands
    def curveTo(from: Int) = new CurveTo(
      commands(from).last,
      commandPos,
      commandPos)

    if (selectedCommand < 0 || commands.isEmpty)
      (shape.setDrawCommands(commands :+ new MoveTo(commandPos)), commands.size)
    else if (selectedCommand == commands.size - 1)
      (shape.setDrawCommands(commands :+ curveTo(selectedCommand)), commands.size)
    else {
      shape.commands(selectedCommand) match {
        case mt: MoveTo =>
          val newComands = (commands.take(selectedCommand) :+
            new MoveTo(commandPos) :+
            new CurveTo(commandPos, mt.pos, mt.pos)) ++ commands.drop(selectedCommand + 1)
          (shape.setDrawCommands(newComands), selectedCommand)
        case ct: CurveTo =>
          commands(selectedCommand + 1) match {
            case _: CurveTo =>
              (shape.setDrawCommands(commands :+ new MoveTo(commandPos)), commands.size)
            case mt: MoveTo =>
              val newComands = (commands.take(selectedCommand + 1) :+
                new CurveTo(ct.end, commandPos, commandPos)) ++
                commands.drop(selectedCommand + 1)
              (shape.setDrawCommands(newComands), selectedCommand + 1)
          }

      }

    }
  }
  def removeCommand(hostPart: CMPart, deltaGroupKey: Int, commandIndex: Int) = {
    val newShapes = hostPart.shapes.map {
      s =>
        val delta = s.deltaLink
        if (delta.key == deltaGroupKey) {
          removeCommande(s, commandIndex)
        } else s
    }
    hostPart.setShapes(newShapes)
  }
  private def interpolateAt(t: Double, p0: Vec, p1: Vec, p2: Vec, p3: Vec): Vec = {
    assert(t >= 0 && t <= 1, "t :" + t)
    val one_t = 1 - t
    val one_t2 = one_t * one_t
    val one_t3 = one_t2 * one_t
    p0 * one_t3 + p1 * 3 * t * one_t2 + p2 * 3 * t * t * one_t + p3 * t * t * t
  }
  def splitBezierCurve(segmentsCount: Int, p0: Vec, p1: Vec, p2: Vec, p3: Vec, startT: Double = 0, endT: Double = 1) = {
    def interrpolateAt(t: Double): Vec = interpolateAt(t, p0, p1, p2, p3)
    val step = (endT - startT) / segmentsCount
    (1 to segmentsCount - 1) map {
      i => (interrpolateAt(step * i + startT), step * i + startT)
    }
  }
  def getCloseSegment(target: Vec, sqrMargin: Double, segments: Seq[(Vec, Double)]): Seq[((Double, Double), Double)] = {
    segments.zip(segments.tail).map {
      case ((v1, f1), (v2, f2)) =>
        val d1 = target <<->> v1
        val d2 = target <<->> v2
        val v12 = (v2 - v1).direction
        val v1t = target - v1
        val dot = v1t.dot(v12)
        val dist = if(dot < 0 || v12.dot(v12) < dot) {
          val d1 = target <<->> v1
          val d2 = target <<->> v2
          d1 min d2
        }else {
          (v1t) <<->> (v12 * (v1t dot v12))
        }
        
        ((f1, f2), dist): ((Double, Double), Double)
    }.filter(_._2 < sqrMargin).sortBy(_._2).take(4)
  }

  private def removeCommande(targetShape: CMShape, commandIndex: Int): CMShape = {
    val oldCommands = targetShape.commands
    val newCommands = {
      if (commandIndex == oldCommands.size - 1)
        oldCommands.dropRight(1)
      else oldCommands(commandIndex) match {
        case mt: MoveTo =>
          val next = oldCommands(commandIndex + 1) match {
            case ct: CurveTo        => new MoveTo(ct.end)
            case other: DrawCommand => other
          }
          (oldCommands.take(commandIndex) :+ next) ++ oldCommands.drop(commandIndex + 2)
        case other =>
          oldCommands.take(commandIndex) ++ oldCommands.drop(commandIndex + 1)
      }
    }

    targetShape.setDrawCommands(newCommands)
  }
  //(1-t)^3 * P0 + 3*(1-t)²*P1 + 3 * (1-t)²*P2 +  t^3 * P3
  // q'(t) = -3 a (1 - t)^2 + 3 b (1 - t)^2 - 6 b (1 - t) t + 6 c (1 - t) t - 3 c t^2 + 3 d t^2
  // = p0*(-3*(1-t)**2) + p1 * (3*(1-t)**2 - 6*t*(1-t)) + p2 *(6*t*(1-t) - 3*t*t) + p3 * 3 * t*t

  class BezierFun(p0: Vec, p1: Vec, p2: Vec, p3: Vec) {
    def pointAt(t: Float) = {
      assert(t >= 0 && t <= 1)
      val one_t = 1 - t
      val one_t2 = one_t * one_t
      val one_t3 = one_t2 * one_t
      p0 * one_t3 + p1 * 3 * t * one_t2 + p2 * 3 * t * t * one_t + p3 * t * t * t
    }
    def tangentAt(t: Float) = {
      val one_t = 1 - t
      val one_t2 = one_t * one_t

      p0 * (-3 * one_t2) +
        p1 * (3 * one_t2 - 6 * t * one_t) +
        p2 * (6 * t * one_t - 3 * t * t) +
        p3 * 3 * t * t
    }

  }
}