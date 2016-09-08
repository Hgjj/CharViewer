package unof.cv.base

import org.scalajs.dom.raw.HTMLImageElement
import unof.cv.base.charLib.CharacterLibrary
import unof.cv.base.charLib.CMLayer
import unof.cv.base.charLib.CMImage
import unof.cv.base.charLib.AlwayVisible
import unof.cv.base.charLib.DeltaLink
import unof.cv.base.charLib.DynamicColor
import unof.cv.base.charLib.CMShape
import unof.cv.base.charLib.VisibleIfNoLink
import unof.cv.base.charLib.LinkedVisibility
import unof.cv.base.charLib.SliderVisibility
import unof.cv.base.charLib.CMPart
import unof.cv.base.charLib.MoveTo
import unof.cv.base.charLib.CurveTo
import scala.scalajs.js
import scala.scalajs.js.Dynamic
import unof.cv.base.charLib.CMCategory
import org.scalajs.dom
import unof.cv.utils.Transforme
import unof.cv.utils.Algebra._
import unof.cv.utils.AllKnownColors
import unof.cv.base.charLib.ImageRef

object CharLibBuilder {
  def apply(options: Seq[JsBodyPart], verbose: Boolean) = {

    var partLinkMap = Map[(String, String), Int]()
    var deltaLinkMap = Map[Int, Int]()
    def keyOfPart(catName: String, partName: String) = partLinkMap.get((catName, partName)) match {
      case Some(key) => key
      case None =>
        val key = CMPart.newLinkKey
        partLinkMap += ((catName, partName) -> key)
        key
    }
    def keyOfLink(linkValue: Int) = deltaLinkMap.get(linkValue) match {
      case Some(key) => key
      case None =>
        val key = DeltaLink.getKey
        deltaLinkMap += (linkValue -> key)
        key
    }
    def parseCondition(s: Seq[String]) = {
      if (s.isEmpty)
        AlwayVisible
      else if (s(0).toLowerCase() == AlwayVisible.key)
        AlwayVisible
      else if (s(0).toLowerCase() == VisibleIfNoLink.key)
        VisibleIfNoLink
      else if (s.length == 3 && s(0).toLowerCase() == LinkedVisibility.key)
        LinkedVisibility(keyOfPart(s(1), s(2)))
      else if (s.length == 4 && s(0).toLowerCase() == SliderVisibility.key) {
        val opp = SliderVisibility.parseOpp(s(2))

        SliderVisibility(s(1), opp, s(3).toInt)
      } else
        throw new IllegalArgumentException(s.mkString("[", ",", "]") + " cannot be parsed to a valid condtion")

    }

    var usedRefs: Set[String] = Set()
    var usedColors: Set[String] = Set()
    var usedSlider: Set[String] = Set()

    def parseShape(c: JSShape): CMShape = {
      def arrToC(arr: js.Array[JSVec]) = {
        arr.toSeq match {
          case Seq(c)           => new MoveTo(c)
          case Seq(c1, c2, end) => new CurveTo(c1, c2, end)
          case _                => throw new IllegalArgumentException("Only 3 or 4 point per curve." + arr + " is not ok.")
        }
      }
      val condition =
        getOrElse(() => parseCondition(c.condition), AlwayVisible)
      def getColors() = {
        c.colorVariables.map {
          col =>
            usedColors += col.boundColor
            new DynamicColor(col.boundColor, AllKnownColors.colorThis(col.constantColor), col.alpha.floatValue())
        }
      }
      val colors =
        getOrElse[Seq[DynamicColor]](getColors, Seq(DynamicColor(), DynamicColor()))

      val delta = getOrElse(() => {
        val d = c.deltaLink
        val k = keyOfLink(d.key.intValue())
        DeltaLink(k, d.slider, d.position.intValue())
      }, DeltaLink())
      if (delta.slider != "None")
        usedSlider += delta.slider
      new CMShape(
        c.points.map(arrToC),
        getOrElse(() => Transforme(c.transform), Transforme()),
        colors,
        getOrElse(() => c.z_layer.floatValue(), 0f),
        condition,
        c.lineWidth.intValue(),
        c.showSurface,
        getOrElse(() => c.lineJoin, "miter"),
        getOrElse(() => c.closed, false),
        delta,
        getOrElse(() => c.name, "A Nameless Shape"))
    }
    def parseImage(c: JSImage): CMImage = {
      usedRefs += c.imageRef
      usedColors += c.colorVariable
      val condition =
        getOrElse(() => parseCondition(c.condition), AlwayVisible)
      val delta = getOrElse(() => {
        val d = c.deltaLink
        val k = keyOfLink(d.key.intValue())
        DeltaLink(k, d.slider, d.position.intValue())
      }, DeltaLink())
      if (delta.slider != "None")
        usedSlider += delta.slider
      new CMImage(
        c.imageRef,
        getOrElse(() => Transforme(c.transform), Transforme()),
        c.colorVariable,
        getOrElse(() => c.z_layer.floatValue(), 0f),
        condition,
        delta,
        getOrElse(() => c.name, c.imageRef))
    }
    def readLayer(d: Dynamic): CMLayer = {
      if (!js.isUndefined(d.imageRef)) {
        val c = d.asInstanceOf[JSImage]
        parseImage(c)
      } else if (!js.isUndefined(d.points)) {
        val c = d.asInstanceOf[JSShape]
        parseShape(c)
      } else throw new Exception("No idea what is this : " + d)
    }

    def getOrElse[A](f: () => A, orElse: A) = {
      try {
        f()
      } catch {
        case t: Throwable =>
          if (verbose)
            t.printStackTrace()
          orElse
      }
    }
    val categories = options.groupBy(_.category).toSeq
      .map {
        case (catName, catOptions) =>
          val options = catOptions.map {
            opt =>
              val partZ = getOrElse(() => opt.partZ.floatValue(), 0f)
              val partTransfom = getOrElse(() => Transforme(opt.partTransform), Transforme())
              var shapes = Seq[CMShape]()
              var images = Seq[CMImage]()
              val components = opt.components
                .foreach {
                  readLayer(_) match {
                    case s: CMShape =>
                      shapes :+= s
                    case i: CMImage =>
                      images :+= i
                  }
                }
              new CMPart(
                opt.partName,
                images.sortBy { _.ref },
                shapes,
                partTransfom,
                partZ,
                keyOfPart(catName, opt.partName))
          }
          new CMCategory(
            catName,
            options.sortBy { _.partName })
      }
    var imagesRefMap = (
      (usedRefs - "None").map((r: String) => (r, new ImageRef(r))) +
      {
        val noneRef = new ImageRef("None")
        val image: HTMLImageElement = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
        noneRef.htmlImage = Some(image)
        ("None", noneRef)
      }).toMap
    val colors = (usedColors.toSet - "None").toSeq
    println("LibBuilder :sliders "+usedSlider)
    new CharacterLibrary(
      categories.sortBy { _.categoryName },
      colors,
      usedSlider.toSeq,
      imagesRefMap)
  }
}