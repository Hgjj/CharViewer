package unof.cv.base.delta

import unof.cv.base.charmaker.CMImage
import unof.cv.base.charmaker.CMImage
import unof.cv.base.charmaker.DrawCommand
import unof.cv.base.charmaker.MoveTo
import unof.cv.base.charmaker.CurveTo
import unof.cv.base.Algebra._
import org.scalajs.dom.raw.HTMLImageElement
import unof.cv.base.charmaker.CMImage
import unof.cv.base.charmaker.CMImage
import unof.cv.base.charmaker.CMImage
import unof.cv.base.charmaker.DynamicColor
import unof.cv.base.charmaker.BoundColor
import unof.cv.base.charmaker.ConstantColor
import unof.cv.base.charmaker.DrawCommand
import unof.cv.base.charmaker.DrawCommand
import unof.cv.base.charmaker.CMImage
import unof.cv.base.charmaker.CMImage
import unof.cv.base.AllKnownColors
import unof.cv.base.CharacterImagePart
import unof.cv.base.Drawer
import unof.cv.base.DrawingContext
import unof.cv.base.ImageRef
import unof.cv.base.Transforme
import unof.cv.base.charmaker.CMShape
import unof.cv.base.charmaker.CMShape
import unof.cv.base.charmaker.CMShape
import unof.cv.base.charmaker.CMShape
import unof.cv.base.charmaker.CMShape
import unof.cv.base.charmaker.CMShape
import unof.cv.base.Character
import unof.cv.base.CharacterShapePart

object DeltaApplier extends Drawer {
  
  def makeImageDif(original: CMImage, delta: CMImage, colorMap: Map[String, String]) = {

    ???

  }
  def makeDiffShape(original: CMShape, delta: CMShape, colorMap: Map[String, String]) = {
    val lci = original.lineColorIndex
    val sci = original.surfaceColorIndex

    val oColors = original.colors
    val dColors = delta.colors
    val (lineColor, lineAlpha) = makeDynamicColorDif(
      oColors(lci),
      dColors(lci),
      colorMap)
    val (surfColor, surfAlpha) = {
      val cd = makeDynamicColorDif(
      oColors(sci),
      dColors(sci),
      colorMap)
      if(delta.showSurcface)
        cd
      else
        (cd._1,-oColors(sci).alpha)
    }
    val dLineWidth = if(original.lineWidth == delta.lineWidth)
      1
    else
      delta.lineWidth / (original.lineWidth).toFloat
    new DiffShape(
      makeCommandsDiff(original.commands, delta.commands),
      makeTransfromDiff(original.transform, delta.transform),
      lineColor,
      lineAlpha,
      surfColor,
      surfAlpha,
      delta.z - original.z,
      dLineWidth,
      original.lineJoint,
      original.closed,
      delta.id)
  }
  def applyDiffShape(to : CMShape, diff : DiffShape,colorMap : Map[String,String], additionalTransforms : Seq[Transforme])= {
    def s (a:Float,b:Float) = 1f min (a + b) max 0f
    def colorSum(d : DynamicColor, c : (Float,Float,Float) ) = {
      val c1 = {
        d match {
          case c : ConstantColor =>
            AllKnownColors.toFloat3(AllKnownColors.colorThis(c.value))
          case b : BoundColor =>
            AllKnownColors.toFloat3(AllKnownColors.colorThis(colorMap(b.value)))
        }
      }
      
      
      "#"+AllKnownColors.toHexaString((
       s(c._1,c1._1),
       s(c._2,c1._2),
       s(c._3,c1._3)
      ))
    }
    val lineColor = to.colors(to.lineColorIndex)
    val surfColor = to.colors(to.surfaceColorIndex)
    new CharacterShapePart(
      colorSum(lineColor, diff.lineColor),
      s(lineColor.alpha, diff.lineAlpha),
      colorSum(surfColor, diff.surfaceColor),
      s(surfColor.alpha,diff.surfaceAlpha),
      (to.lineWidth * diff.lineWidth).toInt,
      to.lineJoint,
      to.showSurcface,
      sumCommandsDiff(to.commands, diff.commands),
      additionalTransforms:+sumTransfromDiff(to.transform, diff.transform),
      diff.locationId,
      to.closed
    )
  }
  def interPolateDiffShape(r1: Float, d1: DiffShape, d2: DiffShape) = {
    if(r1 < 0 || r1 >1)
      throw new IllegalArgumentException("interpolationratio must be in [0,1]." + r1+" isn't.")
    val r2 = 1 - r1
    def t3Inter(a: (Float, Float, Float), b: (Float, Float, Float)) =
      (
        a._1 * r1 + b._1 * r2,
        a._2 * r1 + b._2 * r2,
        a._3 * r1 + b._3 * r2)
        
    val idOfClosest = if(r1 < 0.5)
      d1.locationId
    else
      d2.locationId
    new DiffShape(
      interpolateCommands(r1, d1.commands, d2.commands),
      interpolateTransforms(r1, d1.transform, d2.transform),
      t3Inter(d1.lineColor, d2.lineColor),
      d1.lineAlpha * r1 + d2.lineAlpha * r2,
      t3Inter(d1.surfaceColor, d2.surfaceColor),
      d1.surfaceAlpha * r1 + d2.surfaceAlpha * r2,
      d1.z * r1 + d2.z * r2,
      (math.pow(d1.lineWidth, r1) * math.pow(d2.lineWidth, r2)).toFloat,
      d1.lineJoint,
      d1.closed,
      idOfClosest)
  }
  def sumDiffShape(d1: DiffShape, d2: DiffShape) = {
    def t3Sum(a: (Float, Float, Float), b: (Float, Float, Float)) =
      (
        a._1 + b._1,
        a._2 + b._2,
        a._3 + b._3)
    new DiffShape(
      sumCommandsDiff(d1.commands, d2.commands),
      sumTransfromDiff(d1.transform, d2.transform),
      t3Sum(d1.lineColor, d2.lineColor),
      d1.lineAlpha + d2.lineAlpha,
      t3Sum(d1.surfaceColor, d2.surfaceColor),
      d1.surfaceAlpha + d2.surfaceAlpha,
      d1.z + d2.z,
      d1.lineWidth * d2.lineWidth,
      d1.lineJoint,
      d1.closed,
      d1.locationId)
  }
  def sumAllImagesDifs(difs: Seq[CharacterImagePart]) = {
    ???
  }
  private def makeTransfromDiff(originalTr: Transforme, deltaTr: Transforme) = {
    if (!(originalTr.isInteroplatable && deltaTr.isInteroplatable))
      throw new UnsupportedOperationException("Matrix resulting of multiplication or invertion can't be used for interpolation")
    Transforme(
      deltaTr.sx / originalTr.sx,
      deltaTr.sy / originalTr.sy,
      deltaTr.rotation - originalTr.rotation,
      deltaTr.dx - originalTr.dx,
      deltaTr.dy - originalTr.dy)
  }
  private def sumTransfromDiff(t1: Transforme, t2: Transforme):Transforme = {
    if (!(t1.isInteroplatable && t2.isInteroplatable))
      throw new UnsupportedOperationException("Matrix resulting of multiplication or invertion can't be used for interpolation")
    Transforme(
      t1.sx * t2.sx,
      t1.sy * t2.sy,
      t1.rotation + t2.rotation,
      t1.dx + t2.dx,
      t1.dy + t2.dy)
  }
  private def makeBoundColorDif(
    originalColor: String,
    deltaColor: String,
    colorMap: Map[String, String]) = {
    makeColorDif(colorMap(originalColor), colorMap(deltaColor))
  }
  private def makeDynamicColorDif(
    originalColor: DynamicColor,
    deltaColor: DynamicColor,
    colorMap: Map[String, String]) = {
    def stringVal(d: DynamicColor) = d match {
      case bound: BoundColor    => colorMap(bound.value)
      case const: ConstantColor => const.value
    }
    val difValue = makeColorDif(stringVal(originalColor), stringVal(deltaColor))
    (difValue, deltaColor.alpha - originalColor.alpha)
  }
  private def makeColorDif(
    originalColor: String,
    deltaColor: String) = {
    val oCode = AllKnownColors.colorThis(originalColor)
    val dCode = AllKnownColors.colorThis(deltaColor)
    val c1 = AllKnownColors.toFloat3(oCode)
    val c2 = AllKnownColors.toFloat3(dCode)
    (
      c2._1 - c1._1,
      c2._2 - c1._2,
      c2._3 - c1._3)

  }
  private def makeCommandsDiff(commands1: Seq[DrawCommand], commands2: Seq[DrawCommand]) = {
    def err = throw new UnsupportedOperationException("DeltaShape work only if the number of command is constant")
    if (commands1.size != commands2.size)
      err
    commands1.zip(commands2) map {
      case (oriMt: MoveTo, dMt: MoveTo) =>
        new MoveTo(dMt.pos - oriMt.pos)
      case (oriCt: CurveTo, dCt: CurveTo) =>
        new CurveTo(
          dCt.cp1 - oriCt.cp1,
          dCt.cp2 - oriCt.cp2,
          dCt.end - oriCt.end)
      case _ => err
    }
  }
  private def sumCommandsDiff(commands1: Seq[DrawCommand], commands2: Seq[DrawCommand]) = {
    def err = throw new UnsupportedOperationException("DeltaShape work only if the number of command is constant")
    if (commands1.size != commands2.size)
      err
    commands1.zip(commands2) map {
      case (mt1: MoveTo, mt2: MoveTo) =>
        new MoveTo(mt1.pos + mt2.pos)
      case (ct1: CurveTo, ct2: CurveTo) =>
        new CurveTo(
          ct1.cp1 + ct2.cp1,
          ct1.cp2 + ct2.cp2,
          ct1.end + ct2.end)
      case _ => err
    }
  }
  private def interpolateCommands(
    ratio: Float,
    commands1: Seq[DrawCommand],
    commands2: Seq[DrawCommand]): Seq[DrawCommand] = {
    def err = throw new UnsupportedOperationException("DeltaShape work only if the number of command is constant")
    if (commands1.size != commands2.size)
      err
    val r2 = 1 - ratio
    commands1.zip(commands2) map {
      case (myMt: MoveTo, otherMt: MoveTo) =>
        new MoveTo(myMt.pos * ratio + otherMt.pos * r2)
      case (myCt: CurveTo, otherCt: CurveTo) =>
        new CurveTo(
          myCt.cp1 * ratio + otherCt.cp1 * r2,
          myCt.cp2 * ratio + otherCt.cp2 * r2,
          myCt.end * ratio + otherCt.end * r2)
      case _ => err
    }

  }
  private def interpolateTransforms(
    ratio: Float,
    tr1: Transforme,
    tr2: Transforme) = {

    if (tr1.m11.isNaN() || tr2.m11.isNaN())
      throw new UnsupportedOperationException("Matrix created by multiplication or invertion can't be interpolated")

    val r2 = 1 - ratio
    Transforme(
      math.pow(tr1.sx, ratio) * math.pow(tr2.sx, r2),
      math.pow(tr1.sy, ratio) * math.pow(tr2.sy, r2),
      tr1.rotation * ratio + tr2.rotation * r2,
      tr1.dx * ratio + tr2.dx * r2,
      tr1.dy * ratio + tr2.dy * r2)
  }
  private def interpolateColors(
    ratio: Float,
    color1: String,
    color2: String) = {
    val r2 = 1 - ratio

  }
  private def sumColors(
    ratio1: Float,
    ratio2: Float,
    color1: String,
    color2: String) = {
    val c1 = AllKnownColors.toFloat3(color1)
    val c2 = AllKnownColors.toFloat3(color2)
    "#" + AllKnownColors.toHexaString((
      c1._1 * ratio1 + c2._1 * ratio2,
      c1._2 * ratio1 + c2._2 * ratio2,
      c1._3 * ratio1 + c2._3 * ratio2))

  }
  private def interpolateImages(
    ratio: Float,
    img1: ImageRef,
    img2: ImageRef) = {
    val r2 = 1 - ratio
    sumImages(ratio, r2, img1, img2)

  }
  private def sumImages(
    ratio1: Float,
    ratio2: Float,
    img1: ImageRef,
    img2: ImageRef): ImageRef = {
    val dim = img1.dimensions max img2.dimensions
    val myContext = new DrawingContext("", dim)
    myContext.ctx.globalAlpha = ratio1
    drawImage(img1.htmlImage.get, "white", Nil, myContext)
    myContext.ctx.globalAlpha = ratio2
    drawImage(img2.htmlImage.get, "white", Nil, myContext)
    val r = new ImageRef("interpolated")
    r.htmlImage = Some(myContext.canvasOrig.asInstanceOf[HTMLImageElement])
    r
  }

  private def interpolateNumber(
    ratio: Float,
    n1: Float,
    n2: Float) = n1 * ratio + (1 - ratio) * n2
}