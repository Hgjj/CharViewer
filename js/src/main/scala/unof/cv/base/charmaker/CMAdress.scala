package unof.cv.base.charmaker

object CMAdress {
  def apply(cm: CharMaker, category: Int, part: Int, layer: Int) = {
    val p = cm.getPart(category, part)
    val layerSelect = {
      if (p.images.size > layer || p.shapes.isEmpty)
        SelectImages
      else
        SelectShapes
    }
    new CMAdress(category, part, layer, layerSelect)
  }
  def apply() = new CMAdress(-1, -1, -1, SelectNone)
  def apply(category: Int) = new CMAdress(category, -1, -1, SelectNone)
  def apply(category: Int, part: Int) = new CMAdress(category, part, -1, SelectNone)
  def apply(
    category: Int,
    part: Int,
    layer: Int,
    layerSelect: LayersSelector) = {
    if (category < 0)
      new CMAdress(-1, -1, -1, SelectNone)
    else if (part < 0)
      new CMAdress(category, -1, -1, SelectNone)
    else if (layer < 0)
      new CMAdress(category, part, -1, SelectNone)
    else if (layerSelect == SelectNone)
      new CMAdress(category, part, -1, SelectNone)
    else
      new CMAdress(category, part, layer, layerSelect)
  }
  def apply(
    category: Int,
    part: Int,
    layer: Int,
    cmlayer: CMLayer): CMAdress = CMAdress(category, part, layer, LayersSelector(cmlayer))

  def unapply(a: CMAdress) = Some((a.category, a.part, a.layer, a.layerSelect))
  implicit def toT4(a: CMAdress) = (a.category, a.part, a.layer, a.layerSelect)
}
sealed class CMAdress(
    val category: Int,
    val part: Int,
    val layer: Int,
    val layerSelect: LayersSelector) {

  def getCategory(cm: CharMaker) = {
    if(category<0)
      None
    else
      Some(cm.categories(category))
  }
  def getLayer(cm: CharMaker) = {
    getPart(cm) match {
      case None =>
      case Some(sPart) =>
        layerSelect match {
          case SelectShapes =>
            Some(sPart.shapes(layer))
          case SelectImages =>
            Some(sPart.images(layer))
          case SelectNone =>
            None
        }
    }

  }
  def forSelectedPart(cm: CharMaker)(f: (CMPart) => Unit) = if (part >= 0) {
    f(cm.categories(category).possibleParts(part))
  }
  def getPart(cm: CharMaker) = if (layer >= 0)
    Some(cm.categories(category).possibleParts(part))
  else
    None
  def forSelectedImage(cm: CharMaker)(f: (CMImage) => Unit) = {
    forSelectedPart(cm)(layerSelect.forImages(_)(s => f(s(layer))))
  }
  def forSelectedShape(cm: CharMaker)(f: (CMShape) => Unit) = {
    forSelectedPart(cm)(layerSelect.forShapes(_)(s => f(s(layer))))
  }
  def forWhateverSelected(cm: CharMaker)(f: (CMLayer) => Unit) = {
    forSelectedPart(cm)(layerSelect.forAnyLayers(_)(s => f(s(layer))))
  }

  def forSelected(
    cm: CharMaker,
    fl: (CMLayer) => Unit,
    fp: (CMPart) => Unit,
    fc: (CMCategory) => Unit) = {
    if (layer >= 0) {
      forWhateverSelected(cm)(fl)
    } else if (part >= 0)
      fp(cm.categories(category).possibleParts(part))
    else if (category >= 0)
      fc(cm.categories(category))
  }
  def forSelected(
    cm: CharMaker,
    fi: (CMImage) => Unit,
    fs: (CMShape) => Unit,
    fp: (CMPart) => Unit,
    fc: (CMCategory) => Unit) = {
    if (layer >= 0) {
      forSelectedImage(cm)(fi)
      forSelectedShape(cm)(fs)
    } else if (part >= 0)
      fp(cm.categories(category).possibleParts(part))
    else if (category >= 0)
      fc(cm.categories(category))
  }

  def mapSelected[A](
    cm: CharMaker,
    fl: (CMLayer) => A,
    fp: (CMPart) => A,
    fc: (CMCategory) => A): A = {
    if (layer >= 0) {
      val p = cm.categories(category).possibleParts(part)
      layerSelect match {
        case SelectImages =>
          fl(p.images(layer))
        case SelectShapes =>
          fl(p.shapes(layer))
        case SelectNone =>
          throw new UnsupportedOperationException("Positive layer with slect none")
      }
    } else if (part >= 0)
      fp(cm.categories(category).possibleParts(part))
    else if (category >= 0)
      fc(cm.categories(category))
    else
      throw new NoSuchElementException("Empty selection mapping")
  }
  def mapSelected[A](
    cm: CharMaker,
    fi: (CMImage) => A,
    fs: (CMShape) => A,
    fp: (CMPart) => A,
    fc: (CMCategory) => A): A = {
    if (layer >= 0) {
      val p = cm.categories(category).possibleParts(part)
      layerSelect match {
        case SelectImages =>
          fi(p.images(layer))
        case SelectShapes =>
          fs(p.shapes(layer))
        case SelectNone =>
          throw new UnsupportedOperationException("Positive layer with slect none")
      }
    } else if (part >= 0)
      fp(cm.categories(category).possibleParts(part))
    else if (category >= 0)
      fc(cm.categories(category))
    else
      throw new NoSuchElementException("Empty selection mapping")
  }
  
  def nameSelected(cm : CharMaker) = {
    mapSelected(cm, _.name, _.partName, _.categoryName)
  }
}
object LayersSelector {
  def apply(layer: CMLayer) = {
    layer match {
      case _: CMShape =>
        SelectShapes
      case _: CMImage => SelectImages
    }
  }
}
sealed trait LayersSelector {
  def forImages(part: CMPart)(f: (Seq[CMImage]) => Unit)
  def forShapes(part: CMPart)(f: (Seq[CMShape]) => Unit)
  def forAnyLayers(part: CMPart)(f: (Seq[CMLayer]) => Unit)

  def updateImages(part: CMPart)(f: (Seq[CMImage]) => Seq[CMLayer]): Seq[CMLayer]
  def updateShapes(part: CMPart)(f: (Seq[CMShape]) => Seq[CMLayer]): Seq[CMLayer]
  def updateAnyLayers(part: CMPart)(f: (Seq[CMLayer]) => Seq[CMLayer]): (Seq[CMLayer], Seq[CMLayer])
}

object SelectImages extends LayersSelector {
  def forImages(part: CMPart)(f: (Seq[CMImage]) => Unit) = f(part.images)
  def forShapes(part: CMPart)(f: (Seq[CMShape]) => Unit) = {}
  def forAnyLayers(part: CMPart)(f: (Seq[CMLayer]) => Unit) = f(part.images)

  def updateImages(part: CMPart)(f: (Seq[CMImage]) => Seq[CMLayer]): Seq[CMLayer] =
    f(part.images)
  def updateShapes(part: CMPart)(f: (Seq[CMShape]) => Seq[CMLayer]): Seq[CMLayer] =
    part.shapes
  def updateAnyLayers(part: CMPart)(f: (Seq[CMLayer]) => Seq[CMLayer]): (Seq[CMLayer], Seq[CMLayer]) =
    (f(part.images), part.shapes)
}
object SelectShapes extends LayersSelector {
  def forImages(part: CMPart)(f: (Seq[CMImage]) => Unit) = {}
  def forShapes(part: CMPart)(f: (Seq[CMShape]) => Unit) = f(part.shapes)
  def forAnyLayers(part: CMPart)(f: (Seq[CMLayer]) => Unit) = f(part.shapes)

  def updateImages(part: CMPart)(f: (Seq[CMImage]) => Seq[CMLayer]): Seq[CMLayer] =
    part.images
  def updateShapes(part: CMPart)(f: (Seq[CMShape]) => Seq[CMLayer]): Seq[CMLayer] =
    f(part.shapes)
  def updateAnyLayers(part: CMPart)(f: (Seq[CMLayer]) => Seq[CMLayer]): (Seq[CMLayer], Seq[CMLayer]) =
    (part.images, f(part.shapes))
}
object SelectNone extends LayersSelector {
  def forImages(part: CMPart)(f: (Seq[CMImage]) => Unit) = {}
  def forShapes(part: CMPart)(f: (Seq[CMShape]) => Unit) = {}
  def forAnyLayers(part: CMPart)(f: (Seq[CMLayer]) => Unit) = {}

  def updateImages(part: CMPart)(f: (Seq[CMImage]) => Seq[CMLayer]): Seq[CMLayer] =
    part.images
  def updateShapes(part: CMPart)(f: (Seq[CMShape]) => Seq[CMLayer]): Seq[CMLayer] =
    part.shapes
  def updateAnyLayers(part: CMPart)(f: (Seq[CMLayer]) => Seq[CMLayer]): (Seq[CMLayer], Seq[CMLayer]) =
    (part.images, part.shapes)
}
