package unof.cv.base

import unof.cv.base.charLib.AlwayVisible
import unof.cv.base.charLib.CMImage
import unof.cv.base.charLib.CMLayer
import unof.cv.base.charLib.CMShape
import unof.cv.base.charLib.CharacterLibrary
import unof.cv.base.charLib.DynamicColor
import unof.cv.base.charLib.LinkedVisibility
import unof.cv.base.charLib.SliderVisibility
import unof.cv.base.charLib.VisibleIfNoLink
import unof.cv.utils.Transforme
import unof.cv.utils.AllKnownColors
import unof.cv.base.charLib.ImageRef
import unof.cv.base.charLib.CMImage

object CharacterMaker {
  def apply(lib: CharacterLibrary,
            choices: Seq[Int],
            colorMask: Seq[String],
            sliderValues: Seq[Int],
            charTransforms: Seq[Transforme],
            imageHome: String = "images/")(onload: (Character) => Unit) = {

    val categories = lib.categories
    val sliders = lib.sliders
    def locationMap = lib.locationMap
    val imageMap = lib.imageMap
    val colors = lib.colors

    if (categories.size != choices.size) {
      throw new IllegalArgumentException("You must make one choice per category.")
    }
    val partPerCat = categories.zip(choices).map {
      case (cat, choice) =>
        (cat.possibleParts(choice), cat.categoryName)
    }
    val catPartLocation = categories.indices.zip(choices)
    val catPerKeyMap = partPerCat.map(t => (t._1.linkKey, t._2)).toMap

    val sliderMap = sliders.zip(sliderValues).toMap
    def removeInvisible(s: Seq[CMLayer]) = {
      val (nlCondition, others) =
        s.partition(_.displayCondition == VisibleIfNoLink)
      val (alwayVisible, linked) =
        others.partition(l => l.displayCondition == AlwayVisible || !l.deltaLink.isSource)
      val visibleLinked = linked.filter {
        lin =>
          lin.displayCondition match {
            case LinkedVisibility(partKey) =>
              catPerKeyMap.get(partKey) match {
                case None => false
                case Some(cat) =>
                  val add = locationMap(lin.id)
                  val ctgri = categories(add.category)

                  if (cat == ctgri.categoryName)
                    throw new Exception("The layer " + lin + " in " + cat + " is linked to a concurent choice")
                  true
              }
            case SliderVisibility(sliderName, opp, value) =>
              opp(sliderMap(sliderName), value)
            case _ => ???
          }
      }
      if (visibleLinked.isEmpty)
        nlCondition ++ alwayVisible
      else
        visibleLinked ++ alwayVisible

    }

    val visibleImages = removeInvisible(partPerCat.flatMap(_._1.components))
    val imagePerPart = visibleImages.flatMap {
      case img: CMImage => img.ref :: Nil
      case _            => Nil
    }.map(imageMap)

    val colorMap = if (colors.isEmpty)
      Map(("None" -> "white"))
    else
      colors.zip(colorMask).toMap + ("None" -> "white")

    def makeASingleDiffOfADeltaPile[A <: CMLayer, B](
      source: A,
      pile: Seq[(String, Seq[A])],
      makeDiff: (A, A, Map[String, String]) => B,
      interpolateDiff: (Float, B, B) => B,
      sumDiff: (B, B) => B) = {

      def oneSliderDiff(slide: Seq[A], sliderValue: Int) =
        if (sliderValue == 0)
          None
        else slide.find(_.deltaLink.position == sliderValue) match {
          case Some(s) =>
            Some(makeDiff(source, s, colorMap))
          case None =>
            val (nlCondition, others) =
              slide.partition(_.displayCondition == VisibleIfNoLink)
            val (alwayVisible, linked) =
              others.partition(_.displayCondition == AlwayVisible)
            val visibleLinked = linked.filter {
              lin =>
                lin.displayCondition match {
                  case LinkedVisibility(partKey) =>
                    catPerKeyMap.get(partKey) match {
                      case None => false
                      case Some(cat) =>
                        val add = locationMap(lin.id)
                        val ctgri = categories(add.category)

                        if (cat == ctgri.categoryName)
                          throw new Exception("The layer " + lin + " in " + cat + " is linked to a concurent choice")
                        true
                    }
                  case SliderVisibility(s, o, c) =>
                    o(sliderMap(s), c)
                  case _ => ???
                }
            }
            val activeDeltas = if (visibleLinked.isEmpty)
              nlCondition ++ alwayVisible
            else
              visibleLinked ++ alwayVisible

            if (activeDeltas.isEmpty)
              None
            else {
              val (bellows, aboves) = activeDeltas.span(_.deltaLink.position <= sliderValue)
              val firstAbove = if (aboves.isEmpty) {
                source
              } else {
                val maybe = aboves.head
                if (maybe.deltaLink.position >= 0 && sliderValue <= 0) {
                  source
                } else
                  maybe
              }
              val justBellow = if (bellows.isEmpty) {
                source
              } else {
                val maybe = bellows.last
                if (maybe.deltaLink.position <= 0 && sliderValue >= 0) {
                  source
                } else
                  maybe
              }
              val above = makeDiff(source, firstAbove, colorMap)
              val bellow = makeDiff(source, justBellow, colorMap)
              val r = (sliderValue - justBellow.deltaLink.position) / (firstAbove.deltaLink.position - justBellow.deltaLink.position).toFloat
              Some(interpolateDiff(r, above, bellow))

            }
        }

      val p = pile.flatMap {
        t =>
          val sliderValue = sliderMap(t._1)
          val reasonableSliderValue =
            (t._2.head.deltaLink.position min 0) max sliderValue min (t._2.last.deltaLink.position max 0)
          oneSliderDiff(t._2, reasonableSliderValue)
      }
      if (p.isEmpty)
        None
      else
        Some(p.reduce(sumDiff))

    }

    def parseDynamicColor(d: DynamicColor) = {
      "#" + AllKnownColors.toHexaString(d.toFloat3(colorMap))
    }
    ImageRef.loadAll(imagePerPart, imageHome) {
      charParts =>
        val selection = choices
          .zip(categories)
          .flatMap(t => t._2.possibleParts(t._1).components)

        val (images, shapes) = visibleImages.partition { _.isInstanceOf[CMImage] }
        val linkedImages = images
          .asInstanceOf[Seq[CMImage]]
          .groupBy { _.deltaLink.key }
          .toSeq.unzip._2

        val imageParts = linkedImages.flatMap {
          pile =>
            val slideMap = pile.groupBy { _.deltaLink.slider }
            slideMap.get("None") match {
              case None =>
                Nil
              case Some(Seq(source)) =>
                val orderedDeltaPile = (slideMap - "None").toSeq
                  .map(t => (t._1, t._2.sortBy { _.deltaLink.position }))
                makeASingleDiffOfADeltaPile(
                  source,
                  orderedDeltaPile,
                  DeltaApplier.makeDiffImage(imageMap)_,
                  DeltaApplier.interpolateDiffImage(imageMap(source.ref).htmlImage.get)_,
                  DeltaApplier.sumDiffImage) match {
                    case None =>
                      val color = colorMap(source.boundColor)
                      (new CharacterImagePart(
                        imageMap(source.ref).htmlImage.get,
                        color,
                        Seq(lib.getPart(source).partTransform, source.transform),
                        source.id), source.z + lib.getPart(source).partZ) :: Nil
                    case Some(diff) =>

                      (
                        DeltaApplier.applyDiffImage(imageMap)(
                          source,
                          diff,
                          colorMap,
                          Seq(lib.getPart(source).partTransform)),
                          source.z + lib.getPart(source).partZ + diff.z) :: Nil
                  }
              case Some(s) =>
                throw new Exception("There is multpilesource source for a slider group: " + s.toString())
            }

        }

        /*images.asInstanceOf[Seq[CMImage]].map {
          cmi =>
            (new CharacterImagePart(
              imageMap(cmi.ref).htmlImage.get,
              colorMap(cmi.boundColor),
              Seq(lib.getPart(cmi).partTransform, cmi.transform),
              cmi.id), cmi.z + lib.getPart(cmi).partZ)
        }*/
        val linkedShapes = shapes
          .asInstanceOf[Seq[CMShape]]
          .groupBy { _.deltaLink.key }
          .toSeq.unzip._2

        val shapeParts = linkedShapes.flatMap {
          pile =>
            val slideMap = pile.groupBy { _.deltaLink.slider }
            slideMap.get("None") match {
              case None =>
                Nil
              case Some(Seq(source)) =>
                val orderedDeltaPile = (slideMap - "None").toSeq
                  .map(t => (t._1, t._2.sortBy { _.deltaLink.position }))
                makeASingleDiffOfADeltaPile(
                  source,
                  orderedDeltaPile,
                  DeltaApplier.makeDiffShape,
                  DeltaApplier.interpolateDiffShape,
                  DeltaApplier.sumDiffShape) match {
                    case None =>
                      val lineColor = source.colors(source.lineColorIndex)
                      val surfaceColor = source.colors(source.surfaceColorIndex)
                      (new CharacterShapePart(
                        parseDynamicColor(lineColor),
                        lineColor.alpha,
                        parseDynamicColor(surfaceColor),
                        surfaceColor.alpha,
                        source.lineWidth,
                        source.lineJoint,
                        source.showSurcface,
                        source.commands,
                        Seq(lib.getPart(source).partTransform, source.transform),
                        source.id,
                        source.closed), source.z + lib.getPart(source).partZ) :: Nil
                    case Some(diff) =>

                      (
                        DeltaApplier.applyDiffShape(
                          source,
                          diff,
                          colorMap,
                          Seq(lib.getPart(source).partTransform)),
                          source.z + lib.getPart(source).partZ + diff.z) :: Nil
                  }
              case Some(s) =>
                throw new Exception("There is multpilesource source for a slider group: " + s.toString())
            }

        }
        val sorted = (imageParts ++ shapeParts)
          .sortBy(_._2).unzip._1
        onload(new Character(sorted, charTransforms))
    }

  }
}