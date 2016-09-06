package unof.cv.base.charmaker

import java.util.NoSuchElementException
import scala.scalajs.js
import scala.scalajs.js.Any.jsArrayOps
import scala.scalajs.js.Any.wrapArray
import scala.scalajs.js.Dynamic
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLImageElement
import unof.cv.base.Algebra._
import unof.cv.base.Character
import unof.cv.base.CharacterImagePart
import unof.cv.base.CharacterShapePart
import unof.cv.base.ImageRef
import unof.cv.base.JSImage
import unof.cv.base.JSShape
import unof.cv.base.JSVec
import unof.cv.base.JsBodyPart
import unof.cv.base.Transforme
import unof.cv.base.charmaker.CMAdress.toT4
import unof.cv.base.JsBodyPart
import unof.cv.base.JSShape
import unof.cv.base.JSImage
import unof.cv.base.delta.DeltaApplier

object CharMaker {

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
      val colors = c.colorVariables.map(DynamicColor.apply(_, 1))
      colors foreach {
        case BoundColor(c) =>
          usedColors += c
        case _ =>
      }
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

      new CMImage(
        c.imageRef,
        getOrElse(() => Transforme(c.transform), Transforme()),
        c.colorVariable,
        getOrElse(() => c.z_layer.floatValue(), 0f),
        condition,
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
    new CharMaker(
      categories.sortBy { _.categoryName },
      colors,
      usedSlider.toSeq,
      imagesRefMap)
  }
}

class CharMaker(
    val categories: Seq[CMCategory],
    colorsIn: Seq[String],
    slidersIn: Seq[String],
    val imageMap: Map[String, ImageRef]) {

  val colors = colorsIn.sorted
  val sliders = slidersIn.sorted
  lazy val locationMap = categories.zipWithIndex
    .flatMap {
      case (cat, catIndex) =>
        cat.possibleParts.zipWithIndex.flatMap {
          case (part, partIndex) =>
            part.images.zipWithIndex.map {
              case (image, imIndex) =>
                (image.id, CMAdress(catIndex, partIndex, imIndex, SelectImages))
            } ++
              part.shapes.zipWithIndex.map {
                case (shape, imIndex) =>
                  (shape.id, CMAdress(catIndex, partIndex, imIndex, SelectShapes))
              }
        }

    }.toMap

  lazy val linkKeyMap = categories.flatMap {
    c =>
      c.possibleParts.map {
        p =>
          p.linkKey -> (c.categoryName, p.partName)
      }
  }.toMap
  def colorFieldForChoices(choices: Seq[Int]) = choices zip categories flatMap {
    case (choiceindex, category) =>
      val part = category.possibleParts(choiceindex)
      part.images.map(_.boundColor) +:
        part.shapes.map(_.boundColors)
  }

  private def updatedMap(addedLayers: CMImage*) = {

    imageMap ++ (
      (addedLayers
        .flatMap {
          case img: CMImage => img.ref :: Nil
          case other        => Nil
        }
        .toSet -- imageMap.keySet)
      .toSeq
      .map { s => s -> new ImageRef(s) })
  }
  private def updatedColors(oldColors: Seq[String], unboundColors: Seq[String], newCategories: Seq[CMCategory]) = {
    if (unboundColors.isEmpty)
      colors
    else {
      val rm = unboundColors.filter { s =>
        s != "None" &&
          !newCategories.flatMap {
            _.possibleParts.flatMap {
              _.components.flatMap {
                _.boundColors
              }
            }
          }.contains(s)
      }
      if (!rm.isEmpty)
        oldColors.filter { !rm.contains(_) }
      else
        oldColors

    }

  }
  def defaultChoices = {

    (categories map (p => 0), colors map (c => "white"))
  }
  def getCategory(catName: String) = categories.find { _.categoryName == catName } match {
    case Some(cat) => cat
    case None      => throw new NoSuchElementException("Ther is no category named " + catName)
  }
  def getCategory(layer: CMLayer): CMCategory = {
    val add = locationMap(layer.id)
    categories(add.category)
  }
  def getPart(img: CMLayer): CMPart = {
    val add = locationMap(img.id)
    getPart(add.category, add.part)
  }
  def getPart(category: String, part: String): CMPart = {
    val (cat, pat) = getLocation(category, part)
    categories(cat).possibleParts(pat)
  }
  def getPart(category: Int, part: Int): CMPart = {
    categories(category).possibleParts(part)
  }
  def getShape(category: Int, part: Int, shape: Int) = {
    getPart(category, part).shapes(shape)
  }
  def getImage(category: Int, part: Int, image: Int) = {
    getPart(category, part).images(image)
  }
  def nameThis(t: (Int, Int, Int)) = {
    "(" +
      (
        if (t._1 < 0 || t._1 >= categories.size) {
          "???,???,???"
        } else {
          val cat = categories(t._1)
          cat.categoryName + "," +
            (
              if (t._2 < 0 || t._2 >= cat.possibleParts.size) {
                "???,???"
              } else {
                val part = cat.possibleParts(t._2)
                part.partName + "," +
                  (
                    if (t._3 < 0 || t._3 >= part.components.size) {
                      "???"

                    } else
                      part.components(t._3) match {
                        case img: CMImage => img.ref
                        case other        => "" + other.hashCode()
                      })
              })
        } + ")")

  }
  def getLocation(l: CMLayer) = locationMap(l.id)
  def getLocation(cat: String): Int = categories.indexWhere(_.categoryName == cat)
  def getLocation(cat: String, opt: String): (Int, Int) = {
    val catIndex = getLocation(cat)
    if (catIndex < 0)
      (-1, -1)
    else {
      val optIndex = categories(catIndex).possibleParts.indexWhere { _.partName == opt }
      if (optIndex < 0)
        (catIndex, -1)
      else {
        (catIndex, optIndex)

      }
    }
  }

  def removeRef(ref: CMImage): CharMaker = {
    val pos = getLocation(ref)
    if (pos._3 < 0)
      this
    else
      remove(pos)
  }
  def add(category: CMCategory) = {

    val newCats = (categories :+ category).sortBy(_.categoryName)

    val newMap = updatedMap(category.possibleParts.flatMap(_.images): _*)
    val newColors = extractColorsNames(category) ++ colors
    val newSliders = extractSliderNames(category) ++ sliders
    new CharMaker(
      newCats,
      newColors.toSeq,
      newSliders.toSeq,
      newMap)
  }
  def add(cat: Int, opt: CMPart): CharMaker = {
    val targetCat = categories(cat)
    val newOpts = (targetCat.possibleParts :+ opt).sortBy(_.partName)

    val newCats = categories
      .updated(cat, targetCat.setPart(newOpts))

    val newMap = updatedMap(opt.images: _*)
    val newColors = extractColorsNames(opt) ++ colors
    val newSliders = extractSliderNames(opt) ++ sliders
    new CharMaker(newCats, newColors.toSeq, newSliders.toSeq, newMap)

  }
  def add(cat: Int, opt: Int, img: CMImage): CharMaker = {
    val targetCat = categories(cat)
    val targetOpt = targetCat.possibleParts(opt)
    val addedColor = img.boundColors
    val newImgs = (targetOpt.images :+ img).sortBy { _.ref }
    val newOpts = targetCat.possibleParts
      .updated(opt, targetOpt.setImages(newImgs))

    val newCats = categories
      .updated(cat, targetCat.setPart(newOpts))

    val newMap = updatedMap(img)
    val newColors = extractColorsNames(img) ++ colors
    new CharMaker(newCats, newColors.toSeq, sliders, newMap)

  }
  def add(cat: Int, opt: Int, shape: CMShape): CharMaker = {
    val targetCat = categories(cat)
    val targetOpt = targetCat.possibleParts(opt)
    val addedColor = shape.boundColors
    val newShapes = (targetOpt.shapes :+ shape)
    val newOpts = targetCat.possibleParts
      .updated(opt, targetOpt.setShapes(newShapes))

    val newCats = categories
      .updated(cat, targetCat.setPart(newOpts))

    val newColors = extractColorsNames(shape) ++ colors
    val newSliders = extractSliderNames(shape) ++ sliders
    new CharMaker(newCats, newColors.toSeq, newSliders.toSeq, imageMap)

  }
  def add(cat: Int, opt: Int, layer: CMLayer): CharMaker = layer match {
    case img: CMImage =>
      add(cat, opt, img)
    case shape: CMShape =>
      add(cat, opt, shape)
  }
  def remove(category: Int): CharMaker = {

    val newCats = categories.take(category) ++ categories.drop(category + 1)
    val remainingColors = newCats.flatMap(extractColorsNames).toSet
    val remainingSliders = newCats.flatMap(extractSliderNames).toSet
    new CharMaker(newCats, remainingColors.toSeq, remainingSliders.toSeq, imageMap)
  }
  def remove(cat: Int, opt: Int): CharMaker = {
    val targetCat = categories(cat)
    val targetOpt = targetCat.possibleParts(opt)
    val newOpts = targetCat.possibleParts.take(opt) ++ targetCat.possibleParts.drop(opt + 1)

    val newCats = if (newOpts.isEmpty) {
      categories.take(cat) ++ categories.drop(cat + 1)
    } else {
      categories.updated(cat, targetCat.setPart(newOpts))
    }

    val remainingColors = newCats.flatMap(extractColorsNames).toSet
    val remainingSliders = newCats.flatMap(extractSliderNames).toSet
    new CharMaker(newCats, remainingColors.toSeq, remainingSliders.toSeq, imageMap)

  }
  def remove(adress: CMAdress): CharMaker = {
    val CMAdress(cat: Int, opt: Int, com: Int, select: LayersSelector) = adress
    val targetCat = categories(cat)
    val targetOpt = targetCat.possibleParts(opt)

    val newOpt = select match {
      case SelectImages =>
        val remainingImgs = targetOpt.images.take(com) ++ targetOpt.images.drop(com + 1)
        targetOpt.setImages(remainingImgs)
      case SelectShapes =>
        val remainingShapes = targetOpt.shapes.take(com) ++ targetOpt.shapes.drop(com + 1)
        targetOpt.setShapes(remainingShapes)
      case SelectNone =>
        throw new IllegalArgumentException("You can't ask to not don't delet nothing.")

    }
    val newOpts = if (newOpt.images.isEmpty && newOpt.shapes.isEmpty) {
      targetCat.possibleParts.take(opt) ++ targetCat.possibleParts.drop(opt + 1)
    } else {
      targetCat.possibleParts
        .updated(opt, newOpt)
    }

    val newCats = if (newOpts.isEmpty) {
      categories.take(cat) ++ categories.drop(cat + 1)
    } else {
      categories.updated(cat, targetCat.setPart(newOpts))
    }

    val remainingColors = categories.flatMap(extractColorsNames).toSet
    val remainingSliders = categories.flatMap(extractSliderNames).toSet
    new CharMaker(newCats, remainingColors.toSeq, remainingSliders.toSeq, imageMap)

  }

  def addPart(targetCat: String, part: CMPart) = {
    val (unchangedCats, changedCats) = categories.partition { _.categoryName != targetCat }
    val newCat = changedCats match {
      case Nil => new CMCategory(targetCat, Seq(part))
      case s =>
        val changedCat = s.head
        val oldParts = changedCat.possibleParts
        val fusionIndex = oldParts.indexWhere { _.partName == part.partName }
        val newParts = if (fusionIndex < 0)
          oldParts :+ part
        else {
          val recieving = oldParts(fusionIndex)
          oldParts.updated(
            fusionIndex,
            recieving
              .setImages((recieving.images ++ part.images).sortBy { _.name })
              .setShapes((recieving.shapes ++ part.shapes).sortBy { _.name }))
        }
        s.head.setPart((newParts).sortBy(_.partName))
    }

    val newCats = (unchangedCats :+ newCat).sortBy(_.categoryName)

    val newMap = updatedMap(part.images: _*)
    val newColors = extractColorsNames(part) ++ colors
    val newSliders = extractSliderNames(part) ++ sliders

    new CharMaker(newCats, newColors.toSeq, newSliders.toSeq, newMap)
  }
  def add(targetCat: String, targetPart: String, shape: CMShape) = {

    val (unchangedCats, changedCats) = categories.partition { _.categoryName != targetCat }
    val newCat = changedCats match {
      case Nil => new CMCategory(targetCat, Nil)
      case s   => s.head
    }
    val targetOptions = newCat.possibleParts
    val (unchangedOptions, changedOptions) = targetOptions.partition { _.partName != targetPart }
    val changedOption = changedOptions match {
      case Nil => new CMPart(targetPart, Nil, Nil, Transforme(), 0, CMPart.newLinkKey)
      case s   => s.head
    }
    val newShapes = (changedOption.shapes :+ shape)
    val newOptions = (unchangedOptions :+ changedOption.setShapes(newShapes)).sortBy(_.partName)
    val newCats = (unchangedCats :+ newCat.setPart(newOptions)).sortBy(_.categoryName)

    val newColors = extractColorsNames(shape) ++ colors
    val newSliders = extractSliderNames(shape) ++ sliders

    new CharMaker(newCats, newColors.toSeq, newSliders.toSeq, imageMap)
  }
  def add(targetCat: String, targetPart: String, image: CMImage) = {

    val (unchangedCats, changedCats) = categories.partition { _.categoryName != targetCat }
    val newCat = changedCats match {
      case Nil => new CMCategory(targetCat, Nil)
      case s   => s.head
    }
    val targetOptions = newCat.possibleParts
    val (unchangedOptions, changedOptions) = targetOptions.partition { _.partName != targetPart }
    val changedOption = changedOptions match {
      case Nil => new CMPart(targetPart, Nil, Nil, Transforme(), 0, CMPart.newLinkKey)
      case s   => s.head
    }
    val newImages = (changedOption.images :+ image).sortBy(_.ref)
    val newOptions = (unchangedOptions :+ changedOption.setImages(newImages)).sortBy(_.partName)
    val newCats = (unchangedCats :+ newCat.setPart(newOptions)).sortBy(_.categoryName)

    val newMap = updatedMap(image)
    val newColors = extractColorsNames(image) ++ colors

    new CharMaker(newCats, newColors.toSeq, sliders, newMap)
  }
  def add(targetCat: String, targetPart: String, layer: CMLayer): CharMaker =
    layer match {
      case image: CMImage =>
        add(targetCat, targetPart, image)
      case shape: CMShape =>
        add(targetCat, targetPart, shape)
    }
  def newImage(targetCat: String, targetPart: String, ref: String) = {
    val newComponent = new CMImage(ref, Transforme(1, 1, 0, 0, 0), "None", 0, AlwayVisible, ref)
    add(targetCat, targetPart, newComponent)
  }
  def updated(cat: Int, newCat: CMCategory): CharMaker = {

    val oldCat = categories(cat)
    val newOpts = newCat.possibleParts
    val newCats = categories.updated(cat, newCat)
    val newMap = updatedMap(newOpts.flatMap(_.images): _*)
    val remainingColor = oldCat.possibleParts.flatMap(_.components).map(_.boundColors).foldLeft(colors) {
      (oldColors, unboundColor) =>
        updatedColors(oldColors, unboundColor, newCats)
    }
    val newColors = newCats.flatMap { extractColorsNames }.toSet
    val newSliders = newCats.flatMap { extractSliderNames }.toSet

    new CharMaker(newCats, newColors.toSeq, newSliders.toSeq, newMap)
  }
  def updated(cat: Int, f: (CMCategory) => CMCategory): CharMaker = {

    val newCat = f(categories(cat))
    updated(cat, newCat)
  }
  def updated(cat: Int, opt: Int, f: (CMPart) => CMPart): CharMaker = {
    val newOpt = f(getPart(cat, opt))
    updated(cat, opt, newOpt)
  }
  def updated(cat: Int, opt: Int, newOpt: CMPart) = {

    val oldCat = categories(cat)
    val oldOpt = oldCat.possibleParts(opt)
    val newOpts = oldCat.possibleParts.updated(opt, newOpt)
    val newCat = oldCat.setPart(newOpts)
    val newCats = categories.updated(cat, newCat)
    val newMap = updatedMap(newOpt.images: _*)
    val remainingColor = oldOpt.components.map(_.boundColors).foldLeft(colors) {
      (oldColors, unboundColors) =>
        updatedColors(oldColors, unboundColors, newCats)
    }
    val newColors = newCats.flatMap { extractColorsNames }.toSet
    val newSliders = newCats.flatMap { extractSliderNames }.toSet

    new CharMaker(newCats, newColors.toSeq, newSliders.toSeq, newMap)
  }
  def updated(l: CMLayer, f: (CMLayer) => CMLayer): CharMaker = {
    val loc: CMAdress = locationMap(l.id)
    updated(loc, f)
  }
  def updateImage(cat: Int, opt: Int, com: Int, f: (CMImage) => CMImage): CharMaker = {
    val newImage = f(categories(cat).possibleParts(opt).images(com))
    updateImage(cat, opt, com, newImage)
  }
  def updateImage(cat: Int, opt: Int, com: Int, newImage: CMImage): CharMaker = {
    val oldCat = categories(cat)
    val oldOpt = oldCat.possibleParts(opt)
    val newImages = oldOpt.images.updated(com, newImage)
    val newOpt = oldOpt.setImages(newImages)
    val newOpts = oldCat.possibleParts.updated(opt, newOpt)
    val newCat = oldCat.setPart(newOpts)
    val newCats = categories.updated(cat, newCat)
    val oldColor = oldOpt.components(com).boundColors
    val newMap = updatedMap(newImage)
    val oldImg = oldOpt.images(com)
    val newColors = if (newImage.boundColors == oldImg.boundColors)
      colors
    else
      newCats.flatMap { extractColorsNames }.toSet.toSeq

    new CharMaker(newCats, newColors, sliders, newMap)
  }
  def updateShape(cat: Int, opt: Int, com: Int, f: (CMShape) => CMShape): CharMaker = {

    val newShape = f(categories(cat).possibleParts(opt).shapes(com))
    updateShape(cat, opt, com, newShape)
  }
  def updateShape(cat: Int, opt: Int, com: Int, newShape: CMShape): CharMaker = {
    val oldCat = categories(cat)
    val oldOpt = oldCat.possibleParts(opt)
    val newShapes = oldOpt.shapes.updated(com, newShape)
    val newOpt = oldOpt.setShapes(newShapes)
    val newOpts = oldCat.possibleParts.updated(opt, newOpt)
    val newCat = oldCat.setPart(newOpts)
    val newCats = categories.updated(cat, newCat)

    val oldShape = oldOpt.shapes(com)

    val newColors = if (newShape.boundColors == oldShape.boundColors)
      colors
    else
      newCats.flatMap { extractColorsNames }.toSet.toSeq
    val newSliders = if (extractSliderNames(newShape) == extractSliderNames(oldShape))
      sliders
    else
      newCats.flatMap { extractSliderNames }.toSet.toSeq

    new CharMaker(newCats, newColors, newSliders, imageMap)
  }
  def updated(adress: CMAdress, l: CMLayer): CharMaker = {
    l match {
      case img: CMImage =>
        updateImage(adress.category, adress.part, adress.layer, img)
      case shape: CMShape =>
        updateShape(adress.category, adress.part, adress.layer, shape)
    }
  }
  def updated(adress: CMAdress, img: CMImage): CharMaker = {
    updateImage(adress.category, adress.part, adress.layer, img)
  }
  def updated(adress: CMAdress, shape: CMShape): CharMaker = {

    updateShape(adress.category, adress.part, adress.layer, shape)

  }
  def updated(adress: CMAdress, f: (CMLayer) => CMLayer): CharMaker = {
    adress.layerSelect match {
      case SelectImages =>
        updateImage(adress.category, adress.part, adress.layer, f.asInstanceOf[(CMImage) => CMImage])
      case SelectShapes =>
        updateShape(adress.category, adress.part, adress.layer, f.asInstanceOf[(CMShape) => CMShape])
      case SelectNone =>
        throw new UnsupportedOperationException("Cant update the layer nothing")
    }
  }
  def updated(adress: CMAdress, f: (CMLayer) => CMLayer, g: (CMPart) => CMPart, h: (CMCategory) => CMCategory): CharMaker = {
    if (adress.layer >= 0)
      updated(adress, f)
    else if (adress.part >= 0)
      updated(adress.category, adress.part, g)
    else
      updated(adress.category, h)
  }
  def updated(adress: CMAdress, fi: (CMImage) => CMImage, fs: (CMShape) => CMShape, g: (CMPart) => CMPart, h: (CMCategory) => CMCategory): CharMaker = {
    if (adress.layer >= 0)
      adress.layerSelect match {
        case SelectImages =>
          updateImage(adress.category, adress.part, adress.layer, fi)
        case SelectShapes =>
          updateShape(adress.category, adress.part, adress.layer, fs)
        case _ => this
      }

    else if (adress.part >= 0)
      updated(adress.category, adress.part, g)
    else
      updated(adress.category, h)
  }
  def map(i: (CMImage) => CMImage, s: (CMShape) => CMShape) = {
    var changedCat = false
    val newCat = categories.map {
      c =>
        var changedPart = false
        val newParts = c.possibleParts.map {
          p =>
            var changedLayer = false
            val newShapes = p.shapes map {
              shape =>
                val newS = s(shape)
                if (newS != shape)
                  changedLayer = true
                newS
            }
            val newImages = p.images map {
              image =>
                val newI = i(image)
                if (newI != image)
                  changedLayer = true
                newI
            }
            if (changedLayer) {
              changedPart = true
              p.setImages(newImages).setShapes(newShapes)
            } else {
              p
            }

        }

        if (changedPart) {
          changedCat = true
          c.setPart(newParts)
        } else
          c
    }

    if (changedCat) {
      new CharMaker(newCat, colors, sliders, imageMap)
    } else {
      this
    }
  }
  def enforceLinkConsitancy: CharMaker = {
    def check[A <: CMLayer](a: A): A = {
      val categoryName = getCategory(a).categoryName
      a.displayCondition match {
        case LinkedVisibility(key) =>
          linkKeyMap.get(key) match {
            case None =>
              a.setCondition(AlwayVisible).asInstanceOf[A]
            case Some((catName, partName)) =>
              if (catName == categoryName) {
                a.setCondition(AlwayVisible).asInstanceOf[A]
              } else
                a
          }
        case _ => a
      }
    }
   map(check, check)
  }

  def extractSliderNames(s: CMShape): Set[String] = {
    Set(s.deltaLink.slider)
  }
  def extractSliderNames(p: CMPart): Set[String] =
    p.shapes.flatMap(extractSliderNames).toSet - "None"
  def extractSliderNames(c: CMCategory): Set[String] =
    c.possibleParts.flatMap(extractSliderNames).toSet
  def extractSliderNames: Set[String] =
    categories.flatMap(extractSliderNames).toSet

  def extractColorsNames(l: CMLayer): Set[String] =
    (l.boundColors.toSet - "None")
  def extractColorsNames(p: CMPart): Set[String] =
    p.components.flatMap(extractColorsNames).toSet
  def extractColorsNames(c: CMCategory): Set[String] =
    c.possibleParts.flatMap(extractColorsNames).toSet
  def extractColorsNames: Set[String] =
    categories.flatMap(extractColorsNames).toSet

  def makeChar(
    choices: Seq[Int],
    colorMask: Seq[String],
    sliderValues: Seq[Int],
    charTransforms: Seq[Transforme],
    imageHome: String = "images/")(onload: (Character) => Unit) = {

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
            case SliderVisibility(sliderName, opp, value) =>
              opp(sliderMap(sliderName), value)
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

    def makeASingleDiffOfADeltaPile(source: CMShape, pile: Seq[(String, Seq[CMShape])]) = {

      def oneSliderDiff(slide: Seq[CMShape], sliderValue: Int) =
        if (sliderValue == 0)
          None
        else slide.find(_.deltaLink.position == sliderValue) match {
          case Some(s) =>
            Some(DeltaApplier.makeDiffShape(source, s, colorMap))
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
              val above = DeltaApplier.makeDiffShape(source, firstAbove, colorMap)
              val bellow = DeltaApplier.makeDiffShape(source, justBellow, colorMap)
              val r = (sliderValue - justBellow.deltaLink.position) / (firstAbove.deltaLink.position - justBellow.deltaLink.position).toFloat
              Some(DeltaApplier.interPolateDiffShape(r, above, bellow))

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
        Some(p.reduce(DeltaApplier.sumDiffShape(_, _)))

    }

    def parseDynamicColor(d: DynamicColor) = d match {
      case ConstantColor(c) => c
      case BoundColor(b)    => colorMap(b)
    }
    ImageRef.loadAll(imagePerPart, imageHome) {
      charParts =>
        val selection = choices
          .zip(categories)
          .flatMap(t => t._2.possibleParts(t._1).components)

        val (images, shapes) = visibleImages.partition { _.isInstanceOf[CMImage] }

        val imageParts = images.asInstanceOf[Seq[CMImage]].map {
          cmi =>
            (new CharacterImagePart(
              imageMap(cmi.ref).htmlImage.get,
              colorMap(cmi.boundColor),
              Seq(getPart(cmi).partTransform, cmi.transform),
              cmi.id), cmi.z + getPart(cmi).partZ)
        }
        val linkedShapes = shapes
          .asInstanceOf[Seq[CMShape]]
          .groupBy { _.deltaLink.key }
          .toSeq.unzip._2

        val shapeParts = linkedShapes.map {
          pile =>
            val slideMap = pile.groupBy { _.deltaLink.slider }
            val source = slideMap.get("None") match {
              case None =>
                throw new Exception("There is no source for a slider group.")
              case Some(Seq(src)) => src
              case Some(s) =>
                throw new Exception("There is multpilesource source for a slider group: " + s.toString())
            }
            val orderedDeltaPile = (slideMap - "None").toSeq
              .map(t => (t._1, t._2.sortBy { _.deltaLink.position }))
            makeASingleDiffOfADeltaPile(source, orderedDeltaPile) match {
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
                  Seq(getPart(source).partTransform, source.transform),
                  source.id,
                  source.closed), source.z + getPart(source).partZ)
              case Some(diff) =>
                
                 (
                  DeltaApplier.applyDiffShape(
                    source,
                    diff,
                    colorMap,
                    Seq(getPart(source).partTransform)),
                    source.z + getPart(source).partZ + diff.z)
            }

        }
        val sorted = (imageParts ++ shapeParts)
          .sortBy(_._2).unzip._1
        onload(new Character(sorted, charTransforms))
    }

  }
}


