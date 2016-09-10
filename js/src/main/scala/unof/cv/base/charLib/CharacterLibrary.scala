package unof.cv.base.charLib

import java.util.NoSuchElementException

import unof.cv.base.charLib.CMAdress.toT4
import unof.cv.utils.Transforme

class CharacterLibrary(
    val categories: Seq[CMCategory],
    colorsIn: Seq[String],
    slidersIn: Seq[String],
    val imageMap: Map[String, ImageRef]) {

  val colors = colorsIn.sorted
  val sliders = slidersIn.sorted
  private var myLocaMap: Option[Map[Int, CMAdress]] = None
  def locationMap = myLocaMap match {
    case None =>
      myLocaMap = Some(categories.zipWithIndex
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

        }.toMap)
      myLocaMap.get
    case Some(map) => map
  }
  private var myLKMap: Option[Map[Int, (String, String)]] = None
  def linkKeyMap = myLKMap match {
    case Some(map) => map
    case None =>
      myLKMap = Some(categories.flatMap {
        c =>
          c.possibleParts.map {
            p =>
              p.linkKey -> (c.categoryName, p.partName)
          }
      }.toMap)
      myLKMap.get
  }

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

  def removeRef(ref: CMImage): CharacterLibrary = {
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
    new CharacterLibrary(
      newCats,
      newColors.toSeq,
      newSliders.toSeq,
      newMap)
  }
  def add(cat: Int, opt: CMPart): CharacterLibrary = {
    val targetCat = categories(cat)
    val newOpts = (targetCat.possibleParts :+ opt).sortBy(_.partName)

    val newCats = categories
      .updated(cat, targetCat.setPart(newOpts))

    val newMap = updatedMap(opt.images: _*)
    val newColors = extractColorsNames(opt) ++ colors
    val newSliders = extractSliderNames(opt) ++ sliders
    new CharacterLibrary(newCats, newColors.toSeq, newSliders.toSeq, newMap)

  }
  def add(cat: Int, opt: Int, img: CMImage): CharacterLibrary = {
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
    val newSliders = extractSliderNames(img) ++ sliders
    new CharacterLibrary(newCats, newColors.toSeq, newSliders.toSeq, newMap)

  }
  def add(cat: Int, opt: Int, shape: CMShape): CharacterLibrary = {
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
    new CharacterLibrary(newCats, newColors.toSeq, newSliders.toSeq, imageMap)

  }
  def add(cat: Int, opt: Int, layer: CMLayer): CharacterLibrary = layer match {
    case img: CMImage =>
      add(cat, opt, img)
    case shape: CMShape =>
      add(cat, opt, shape)
  }
  def remove(category: Int): CharacterLibrary = {

    val newCats = categories.take(category) ++ categories.drop(category + 1)
    val remainingColors = newCats.flatMap(extractColorsNames).toSet
    val remainingSliders = newCats.flatMap(extractSliderNames).toSet
    new CharacterLibrary(newCats, remainingColors.toSeq, remainingSliders.toSeq, imageMap)
  }
  def remove(cat: Int, opt: Int): CharacterLibrary = {
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
    new CharacterLibrary(newCats, remainingColors.toSeq, remainingSliders.toSeq, imageMap)

  }
  def remove(adress: CMAdress): CharacterLibrary = {
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
    new CharacterLibrary(newCats, remainingColors.toSeq, remainingSliders.toSeq, imageMap)

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

    new CharacterLibrary(newCats, newColors.toSeq, newSliders.toSeq, newMap)
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

    new CharacterLibrary(newCats, newColors.toSeq, newSliders.toSeq, imageMap)
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
    
    val newSliders = extractSliderNames(image) ++ sliders

    new CharacterLibrary(newCats, newColors.toSeq, newSliders.toSeq, newMap)
  }
  def add(targetCat: String, targetPart: String, layer: CMLayer): CharacterLibrary =
    layer match {
      case image: CMImage =>
        add(targetCat, targetPart, image)
      case shape: CMShape =>
        add(targetCat, targetPart, shape)
    }
  def newImage(targetCat: String, targetPart: String, ref: String) = {
    val newComponent = new CMImage(ref, Transforme(1, 1, 0, 0, 0), "None", 0, AlwayVisible, DeltaLink(),1, ref)
    add(targetCat, targetPart, newComponent)
  }
  def updated(cat: Int, newCat: CMCategory): CharacterLibrary = {

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

    new CharacterLibrary(newCats, newColors.toSeq, newSliders.toSeq, newMap)
  }
  def updated(cat: Int, f: (CMCategory) => CMCategory): CharacterLibrary = {

    val newCat = f(categories(cat))
    updated(cat, newCat)
  }
  def updated(cat: Int, opt: Int, f: (CMPart) => CMPart): CharacterLibrary = {
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

    new CharacterLibrary(newCats, newColors.toSeq, newSliders.toSeq, newMap)
  }
  def updated(l: CMLayer, f: (CMLayer) => CMLayer): CharacterLibrary = {
    val loc: CMAdress = locationMap(l.id)
    updated(loc, f)
  }
  def updateImage(cat: Int, opt: Int, com: Int, f: (CMImage) => CMImage): CharacterLibrary = {
    val newImage = f(categories(cat).possibleParts(opt).images(com))
    updateImage(cat, opt, com, newImage)
  }
  def updateImage(cat: Int, opt: Int, com: Int, newImage: CMImage): CharacterLibrary = {
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

    val newSliders = if (extractSliderNames(newImage) == extractSliderNames(oldImg))
      sliders
    else
      newCats.flatMap { extractSliderNames }.toSet.toSeq

    new CharacterLibrary(newCats, newColors, newSliders, newMap)
  }
  def updateShape(cat: Int, opt: Int, com: Int, f: (CMShape) => CMShape): CharacterLibrary = {

    val newShape = f(categories(cat).possibleParts(opt).shapes(com))
    updateShape(cat, opt, com, newShape)
  }
  def updateShape(cat: Int, opt: Int, com: Int, newShape: CMShape): CharacterLibrary = {
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

    new CharacterLibrary(newCats, newColors, newSliders, imageMap)
  }
  def updated(adress: CMAdress, l: CMLayer): CharacterLibrary = {
    l match {
      case img: CMImage =>
        updateImage(adress.category, adress.part, adress.layer, img)
      case shape: CMShape =>
        updateShape(adress.category, adress.part, adress.layer, shape)
    }
  }
  def updated(adress: CMAdress, img: CMImage): CharacterLibrary = {
    updateImage(adress.category, adress.part, adress.layer, img)
  }
  def updated(adress: CMAdress, shape: CMShape): CharacterLibrary = {

    updateShape(adress.category, adress.part, adress.layer, shape)

  }
  def updated(adress: CMAdress, f: (CMLayer) => CMLayer): CharacterLibrary = {
    adress.layerSelect match {
      case SelectImages =>
        updateImage(adress.category, adress.part, adress.layer, f.asInstanceOf[(CMImage) => CMImage])
      case SelectShapes =>
        updateShape(adress.category, adress.part, adress.layer, f.asInstanceOf[(CMShape) => CMShape])
      case SelectNone =>
        throw new UnsupportedOperationException("Cant update the layer nothing")
    }
  }
  def updated(adress: CMAdress, f: (CMLayer) => CMLayer, g: (CMPart) => CMPart, h: (CMCategory) => CMCategory): CharacterLibrary = {
    if (adress.layer >= 0)
      updated(adress, f)
    else if (adress.part >= 0)
      updated(adress.category, adress.part, g)
    else
      updated(adress.category, h)
  }
  def updated(adress: CMAdress, fi: (CMImage) => CMImage, fs: (CMShape) => CMShape, g: (CMPart) => CMPart, h: (CMCategory) => CMCategory): CharacterLibrary = {
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
      new CharacterLibrary(newCat, colors, sliders, imageMap)
    } else {
      this
    }
  }
  def enforceLinkConsitancy: CharacterLibrary = {
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

  def extractSliderNames(l: CMLayer): Set[String] = {
    Set(l.deltaLink.slider) - "None"
  }
  def extractSliderNames(p: CMPart): Set[String] =
    p.components.flatMap(extractSliderNames).toSet
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

}


