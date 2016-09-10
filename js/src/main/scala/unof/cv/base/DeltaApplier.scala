package unof.cv.base

import org.scalajs.dom.raw.HTMLImageElement

import unof.cv.base.charLib.CMImage
import unof.cv.base.charLib.CMShape
import unof.cv.base.charLib.CurveTo
import unof.cv.base.charLib.DrawCommand
import unof.cv.base.charLib.DynamicColor
import unof.cv.base.charLib.MoveTo
import unof.cv.utils.Algebra.DDVector
import unof.cv.utils.AllKnownColors
import unof.cv.utils.Transforme
import unof.cv.utils.Algebra._
import unof.cv.base.charLib.ImageRef

object DeltaApplier extends Drawer {

  def makeDiffImage(imageMap: Map[String, ImageRef])(original: CMImage, delta: CMImage, colorMap: Map[String, String]) = {

    val dColor = makeColorDif(colorMap(original.boundColor), colorMap(delta.boundColor))
    val oSrc = original.ref
    val dSrc = delta.ref
    val ref = if(dSrc == "None")
      None
    else
      imageMap.get(dSrc).flatMap(_.htmlImage)
    val a = delta.alpha - original.alpha
  
    new DiffImage(
      ref,
      makeTransfromDiff(original.transform, delta.transform),
      dColor,
      delta.z - original.z,
      delta.id,
      a)

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
      if (delta.showSurcface)
        cd
      else
        (cd._1, -oColors(sci).alpha)
    }
    val dLineWidth = if (original.lineWidth == delta.lineWidth)
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

  def applyDiffShape(to: CMShape, diff: DiffShape, colorMap: Map[String, String], additionalTransforms: Seq[Transforme]) = {
    def s(a: Float, b: Float) = 1f min (a + b) max 0f
    def colorSum(d: DynamicColor, c: (Float, Float, Float)) = {
      val c1 = d.toFloat3(colorMap)

      "#" + AllKnownColors.toHexaString((
        s(c._1, c1._1),
        s(c._2, c1._2),
        s(c._3, c1._3)))
    }
    val lineColor = to.colors(to.lineColorIndex)
    val surfColor = to.colors(to.surfaceColorIndex)
    new CharacterShapePart(
      colorSum(lineColor, diff.lineColor),
      s(lineColor.alpha, diff.lineAlpha),
      colorSum(surfColor, diff.surfaceColor),
      s(surfColor.alpha, diff.surfaceAlpha),
      (to.lineWidth * diff.lineWidth).toInt,
      to.lineJoint,
      to.showSurcface,
      sumCommandsDiff(to.commands, diff.commands),
      additionalTransforms :+ sumTransfromDiff(to.transform, diff.transform),
      diff.locationId,
      to.closed)
  }
  def applyDiffImage(imageMap: Map[String, ImageRef])(to: CMImage, diff: DiffImage, colorMap: Map[String, String], additionalTransforms: Seq[Transforme]) = {
    /*
     * Img(a) => Img(b) = Img(b)
     * Img(a) => Img(None) = Img(a)
     * */
    
    val oColor = AllKnownColors.toFloat3(AllKnownColors.colorThis(colorMap(to.boundColor)))
    val diffColor = diff.color
    val f3ResColor = (
      oColor._1 + diffColor._1,
      oColor._2 + diffColor._2,
      oColor._3 + diffColor._3)

    val img = diff.image match {
      case None    => imageMap(to.ref).htmlImage.get
      case Some(i) => i
    }
    new CharacterImagePart(
      img,
      "#" + AllKnownColors.toHexaString(f3ResColor),
      additionalTransforms :+ sumTransfromDiff(to.transform, diff.transform),
      diff.locationId,
      to.alpha + diff.dAlpha)
  }
  def interpolateDiffShape(r1: Float, d1: DiffShape, d2: DiffShape) = {
    if (r1 < 0 || r1 > 1)
      throw new IllegalArgumentException("interpolationratio must be in [0,1]." + r1 + " isn't.")
    val r2 = 1 - r1
    def t3Inter(a: (Float, Float, Float), b: (Float, Float, Float)) =
      (
        a._1 * r1 + b._1 * r2,
        a._2 * r1 + b._2 * r2,
        a._3 * r1 + b._3 * r2)

    val idOfClosest = if (r1 < 0.5)
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
  def interpolateDiffImage(r1: Float, d1: DiffImage, d2: DiffImage) = {
    if (r1 < 0 || r1 > 1)
      throw new IllegalArgumentException("interpolationratio must be in [0,1]." + r1 + " isn't.")
   
    val r2 = 1 - r1
    def t3Inter(a: (Float, Float, Float), b: (Float, Float, Float)) =
      (
        a._1 * r1 + b._1 * r2,
        a._2 * r1 + b._2 * r2,
        a._3 * r1 + b._3 * r2)
    val idOfClosest = if (r1 < 0.5)
      d1.locationId
    else
      d2.locationId
    val interpolatedImage = (d1.image, d2.image) match {
      case (None, None)             => None
      case (Some(img), None)        => None
      case (None, Some(img))        => None
      case (Some(img1), Some(img2)) => Some(interpolateImages(r1, img1, img2))
    }
    new DiffImage(
      interpolatedImage,
      interpolateTransforms(r1, d1.transform, d2.transform),
      t3Inter(d1.color, d2.color),
      d1.z * r1 + d2.z * r2,
      idOfClosest,
      d1.dAlpha * r1 + d2.dAlpha * r2)

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
  def sumDiffImage(d1: DiffImage, d2: DiffImage) = {

    def t3Sum(a: (Float, Float, Float), b: (Float, Float, Float)) =
      (
        a._1 + b._1,
        a._2 + b._2,
        a._3 + b._3)
    val summedImage = d1.image match {
      case None => d2.image
      case Some(img1) =>
        d2.image match {
          case None =>
            Some(img1)
          case Some(img2) =>
            if (img1 == img2)
              Some(img1)
            else
              Some(interpolateImages(0.5f, img1, img2))
        }
    }
    new DiffImage(
      summedImage,
      sumTransfromDiff(d1.transform, d2.transform),
      t3Sum(d1.color, d2.color),
      d1.z + d2.z,
      d1.locationId,
      d1.dAlpha + d2.dAlpha)

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
  private def sumTransfromDiff(t1: Transforme, t2: Transforme): Transforme = {
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

    def minus(t1: (Float, Float, Float), t2: (Float, Float, Float)) =
      (t1._1 - t2._1,
        t1._2 - t2._2,
        t1._3 - t2._3)
    val oValue = originalColor.toFloat3(colorMap)
    val dValue = deltaColor.toFloat3(colorMap)
    (minus(dValue, oValue), deltaColor.alpha - originalColor.alpha)
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
    img1: HTMLImageElement,
    img2: HTMLImageElement) = {
    val r2 = 1 - ratio
    sumImages(ratio, r2, img1, img2)

  }
  private def sumImages(
    ratio1: Float,
    ratio2: Float,
    img1: HTMLImageElement,
    img2: HTMLImageElement): HTMLImageElement = {
    val dim = (img1.width, img1.height) max (img2.width, img2.height)
    val myContext = new DrawingContext("", dim)
    drawImage(img1, "white", Nil, ratio1, myContext)
    drawImage(img2, "white", Nil, ratio2, myContext)
    myContext.canvasOrig.asInstanceOf[HTMLImageElement]

  }

  private def interpolateNumber(
    ratio: Float,
    n1: Float,
    n2: Float) = n1 * ratio + (1 - ratio) * n2
}