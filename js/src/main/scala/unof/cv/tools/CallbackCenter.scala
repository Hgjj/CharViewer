package unof.cv.tools

import org.scalajs.jquery.JQueryEventObject
import scala.scalajs.js.Dynamic
import org.scalajs.jquery.jQuery
import scala.annotation.tailrec
import org.scalajs.dom.raw.HTMLElement
import unof.cv.base.Algebra._
import org.scalajs.dom.raw.Element
import org.scalajs.dom.raw.MouseEvent
import org.scalajs.dom.raw.Event
import unof.cv.base.DrawChar
import unof.cv.base.Character
import unof.cv.base.Transforme
import unof.cv.base.DrawingContext
import unof.cv.base.charmaker.CharMaker
import unof.cv.base.charmaker.CMImage
import unof.cv.base.charmaker.CMPart
import unof.cv.base.charmaker.CMCategory
import org.scalajs.dom.raw.KeyboardEvent
import unof.cv.base.charmaker.AlwayVisible
import unof.cv.base.charmaker.VisibilityCondition
import unof.cv.base.charmaker.CMLayer
import unof.cv.base.charmaker.CMImage
import unof.cv.base.charmaker.CMImage
import unof.cv.base.charmaker.CMShape
import unof.cv.base.charmaker.DynamicColor
import unof.cv.base.charmaker.CMShape
import unof.cv.base.charmaker.ConstantColor
import unof.cv.base.charmaker.ConstantColor
import org.scalajs.dom.raw.WheelEvent
import unof.cv.base.charmaker.CMShape
import unof.cv.base.charmaker.SelectImages
import unof.cv.base.charmaker.SelectShapes
import unof.cv.base.charmaker.LayersSelector
import unof.cv.base.charmaker.CMAdress
import unof.cv.base.charmaker.SelectNone
import unof.cv.base.charmaker.CMShape
import unof.cv.base.charmaker.CMShape
import unof.cv.base.charmaker.CMImage
import unof.cv.base.charmaker.CMShape
import unof.cv.base.charmaker.LayersSelector
import unof.cv.tools.paramsmenu.ParMenuDrawer
import unof.cv.base.charmaker.DrawCommand
import unof.cv.base.charmaker.CurveTo
import unof.cv.base.charmaker.CurveTo
import unof.cv.base.charmaker.MoveTo
import unof.cv.base.charmaker.CurveTo
import unof.cv.base.charmaker.CMShape

class CallbackCenter(
    startingChoices: Seq[Int],
    startingColorMask: Seq[String],
    startingSliderValues: Seq[Int],
    startingSelection: Seq[Int],
    val charContext: DrawingContext,
    startingCharMaker: CharMaker,
    setting: CvSetting) {
  val devMod = setting.devMod
  private var partHeld = false
  private var dragAll = false
  private var distHeldMouse: Vec = (0, 0)

  private var ctrlIsDown = false;
  private var shiftIsDown = false;

  private var selectedCurveComand = -1
  private var draggedHandle: Option[(Int, Int)] = None

  private val undoButton = jQuery(setting.undoButton)
  private val redoButton = jQuery(setting.redoButton)

  private val wheelZoomSpeed = setting.wheelZoomSpeed.floatValue()
  redoButton.hide()

  private def startingAdress = if (startingSelection.size < 4)
    CMAdress(startingCharMaker, startingSelection(0), startingSelection(1), startingSelection(2))
  else if (startingSelection(3) == 0)
    CMAdress(startingSelection(0), startingSelection(1), startingSelection(2), SelectImages)
  else
    CMAdress(startingSelection(0), startingSelection(1), startingSelection(2), SelectShapes)

  private val stat = new AppStat(
    startingChoices,
    startingColorMask,
    startingSliderValues,
    startingAdress,
    startingCharMaker: CharMaker,
    Transforme(),
    None)
  CMPrinter.makePeriodicalCookieSaves(setting.cookiSavePeriode.intValue(), stat)
  def choices = stat.choices
  private def choices_=(s: Seq[Int]) = {
    stat.choices = s
    undoButton.show()
    redoButton.hide()
  }

  def colorMask = stat.colorMask
  def colorMask_=(s: Seq[String]) = {
    stat.colorMask = s
    undoButton.show()
    redoButton.hide()
  }

  def selection = stat.selection
  private def selection_=(a: CMAdress) = {
    stat.selection = a
    undoButton.show()
    redoButton.hide()
  }

  def charMaker = stat.charMaker
  private def charMaker_=(cm: CharMaker) {
    resetSliders
    validateChoices(stat.charMaker, cm)
    validateSelection(cm)
    validateColors(stat.charMaker, cm)
    validateSlider(stat.charMaker, cm)
    stat.charMaker = cm
    undoButton.show()
    redoButton.hide()
  }
  def globalTransform = stat.globalTransform
  private def globalTransform_=(t: Transforme) = {
    stat.globalTransform = t
    undoButton.show()
    redoButton.hide()
  }
  def selectedShape = stat.selectedShape
  private def selectedShape_=(s: Option[(Int, Int, Int)]) = {
    stat.selectedShape = s
    undoButton.show()
    redoButton.hide()
  }
  def slidersValues = stat.slidersValues
  private def slidersValues_=(s: Seq[Int]) = {
    stat.slidersValues = s
    undoButton.show()
    redoButton.hide()
  }

  private var zSorterScale = -1;
  val colorDivName: String = setting.colorsMenuComponent
  val partMenuName: String = setting.partsMenuComponent
  val fullPartmenuWidth: Int = setting.partsMenuFullWidth.intValue()

  validateColors(charMaker, charMaker)
  validateSlider(charMaker, charMaker)
  validateSelection(charMaker)
  validateChoices(charMaker, charMaker)

  stat.forgetPast
  undoButton.hide()

  //val zContext = new DrawingContext(setting.zLayerSorterCanva);

  jQuery(setting.saveButton).click(onSave _)
  jQuery(setting.cookieSaveButton).click(onCookieSave _)

  undoButton.click((evt: JQueryEventObject) => undo)
  redoButton.click((evt: JQueryEventObject) => redo)

  charContext.canvasElem.addEventListener("mousedown", onCharacterClicked _)
  charContext.canvasElem.addEventListener("mouseup", onMouseUp _)
  charContext.canvasElem.addEventListener("mouseexit", onMouseUp _)
  charContext.canvasElem.addEventListener("mousemove", onMouseMove _)
  charContext.canvasElem.addEventListener("wheel", onMouseWheel _)

  Dynamic.global.document.addEventListener("keydown", onKeyPressed _)
  Dynamic.global.document.addEventListener("keyup", onKeyUp _)

  DrawMenu.createMenu(setting, this)
  ColorMenu.createMenuColor(setting, this)
  ColorMenu.drawColorMenu(setting, colorMask, charMaker.colors)
  SlidersMenu.create(this, setting)
  SlidersMenu.update(this, setting)
  ParMenuDrawer.bindComponents(setting, this)
  DrawMenu.updateMenu(charMaker, charMaker, Seq.fill(choices.size)(0), choices, this, setting)
  ParMenuDrawer.update(setting, this)

  jQuery("body").click(refreshColorsOnFirstClick _);

  curentCharacter {
    c =>
      DrawChar(c, charContext)
  }

  private def curentCharacter(onload: (Character) => Unit) = charMaker.makeChar(choices, colorMask, slidersValues, Seq(globalTransform))(onload)

  private def updateChar = curentCharacter { drawChar }
  private def drawChar(c: Character) = {
    DrawChar(c, charContext)
    drawHandlesIfNeeded
  }
  def refreshColorsOnFirstClick(evt: JQueryEventObject): Any = {
    updateAll(charMaker, choices)
    jQuery("body").off();
  }

  private def drawHandlesIfNeeded {
    selectedShape match {
      case None =>
      case Some((c, p, s)) =>
        val part = charMaker.getPart(c, p)
        val shape = part.shapes(s)
        ShapeManipulator.drawShapeHandles(
          shape,
          Seq(globalTransform, part.partTransform),
          charContext,
          setting,
          selectedCurveComand)
    }
  }
  private def updateAll(oldCM: CharMaker, oldChoices: Seq[Int]) {

    ColorMenu.updateColorColors(colorMask)
    DrawMenu.updateMenu(oldCM, charMaker, oldChoices, choices, this, setting)
    ColorMenu.drawColorMenu(setting, charMaker.colors, colorMask)
    SlidersMenu.update(this, setting)
    ParMenuDrawer.update(setting, this)
    curentCharacter {
      c =>
        DrawChar(c, charContext)
    }
    updateChar
  }

  private def validateSelection(newCm: CharMaker) = {
    if (selection.category >= 0) {
      if (selection.category >= newCm.categories.size) {
        selection = CMAdress()
      } else if (selection.category >= 0) {
        if (selection.part >= newCm.categories(selection.category).possibleParts.size) {
          selection = CMAdress(selection.category)
        } else if (selection.layer >= 0) {
          selection.forSelectedPart(newCm) {
            p =>
              selection.layerSelect.forAnyLayers(p) {
                l =>
                  if (selection.layer >= l.size)
                    selection = CMAdress(selection.category, selection.part)
              }
          }

        }
      }

    }

  }
  private def validateColors(oldCm: CharMaker, newCm: CharMaker) =
    validateStuff(oldCm, newCm, _.colors, "white", colorMask, colorMask_=)
  private def validateSlider(oldCm: CharMaker, newCm: CharMaker) =
    validateStuff(oldCm, newCm, _.sliders, 0, slidersValues, slidersValues_=)
  private def validateStuff[A](oldCm: CharMaker,
                               newCm: CharMaker,
                               getList: (CharMaker) => Seq[String],
                               default: A,
                               managedList: Seq[A],
                               updateList: (Seq[A]) => Unit): Unit = {

    val oldList = getList(oldCm)
    val newList = getList(newCm)
    if (newList.isEmpty) {
      updateList(Nil)
    } else if (oldList.isEmpty) {
      updateList(Seq.fill(newList.size)(default))
    } else if (managedList.isEmpty) {
      updateList(Seq.fill(newList.size)(default))
    } else {
      val newS = newList.size
      val oldS = oldList.size
      updateList(if (oldS != newS) {
        val dif = oldList.zip(newList).indexWhere(t => t._1 != t._2)
        if (oldS > newS) {
          if (dif < 0)
            managedList.dropRight(1)
          else
            managedList.take(dif) ++ managedList.drop(dif + 1)

        } else {
          if (dif < 0)
            managedList :+ (default)
          else
            (managedList.take(dif) :+ (default)) ++ managedList.drop(dif)
        }

      } else managedList.reverse.padTo(newS, default).reverse)
    }

  }
  private def validateChoices(oldCm: CharMaker, newCm: CharMaker) = {
    val zi = (oldCm.categories.map(_.categoryName) zip newCm.categories.map { _.categoryName })
    val dif = zi.indexWhere(t => t._1 != t._2)
    if (choices.exists { _ < 0 })
      throw new IllegalArgumentException("Can't choose something lesser than 0")
    if (oldCm.categories.size > newCm.categories.size) {

      if (dif < 0)
        choices = choices.dropRight(1)
      else
        choices = choices.take(dif) ++ choices.drop(dif + 1)
    } else if (oldCm.categories.size < newCm.categories.size) {
      if (dif < 0) {
        choices :+= 0
      } else
        choices = (choices.take(dif) :+ 0) ++ choices.drop(dif)
    } else if (dif > 0) {
      val choiceMap = (oldCm.categories.map(_.categoryName).zip(choices)).toMap.withDefault(x => 0)
      choices = newCm.categories.map(c => choiceMap(c.categoryName))
    }

    choices = choices.take(newCm.categories.size).zip(newCm.categories) map {
      t => t._1 min t._2.possibleParts.size - 1
    }
  }
  def currentOptions = charMaker
  def currentSelection = selection
  def currentChoices = choices
  def currentColors = colorMask

  private def undo = {
    val oldCm = charMaker
    val oldChoices = choices
    stat.undo
    updateAll(oldCm, oldChoices)
    if (!stat.canUndo)
      undoButton.hide()
    if (stat.canRedo)
      redoButton.show()
  }
  private def redo = {
    val oldCm = charMaker
    val oldChoices = choices
    stat.redo
    updateAll(oldCm, oldChoices)
    if (!stat.canRedo)
      redoButton.hide()
    if (stat.canUndo)
      undoButton.show()
  }
  def isCrtl(key: String) = {
    val lowKey = key.toLowerCase()
    lowKey == "ctrl" || lowKey == "meta" || lowKey == "control"
  }
  def onKeyUp(evt: KeyboardEvent): Any = {
    val lowKey = evt.key.toLowerCase()
    if (lowKey == "shift") {
      shiftIsDown = false;
      dragAll = false;
    } else if (isCrtl(evt.key)) {
      ctrlIsDown = false;
    }

  }
  def onKeyPressed(evt: KeyboardEvent): Any = {
    if (evt.ctrlKey || evt.metaKey || isCrtl(evt.key)) {
      ctrlIsDown = true;
      if (evt.key == "z") {
        undo
      } else if (evt.key == "y") {
        redo

      }
    }
    if (evt.shiftKey || evt.key.toLowerCase() == "shift") {
      shiftIsDown = true;
    }
  }
  def onCharacterClicked(evt: MouseEvent): Any = {
    if (shiftIsDown || !devMod) {
      val tr = globalTransform
      distHeldMouse = (componentCoord((evt.pageX, evt.pageY), charContext.canvasElem) - (tr.dx, tr.dy)) / (tr.sx, tr.sy)
      dragAll = true
    } else if (selectedShape.isDefined) {
      selectedShape match {
        case None => standardPicking
        case Some((c, p, l)) =>
          val part = charMaker.getPart(c, p)
          val s = part.shapes(l)

          draggedHandle = ShapeManipulator.click(
            componentCoord((evt.pageX, evt.pageY), charContext.canvasElem),
            s,
            Seq(globalTransform, part.partTransform),
            setting,
            selectedCurveComand)
          draggedHandle match {
            case None =>
              if (ctrlIsDown) {
                val invertScreenMatrix = (globalTransform * part.partTransform * s.transform).invert
                val localMousePos = invertScreenMatrix * componentCoord((evt.pageX, evt.pageY), charContext.canvasElem)

                onShapeRecivingNewCommand(localMousePos, c, p, l)
              } else
                standardPicking
            case Some((curve, handle)) =>
              selection = CMAdress(c, p, l, SelectShapes)
              if (handle == 0) {
                println("Callbacks , handle : " + handle)
                selectedCurveComand = curve
                ParMenuDrawer.update(setting, this)
                if (ctrlIsDown) {
                  onShapeLoosingComande(c, p, l, curve)
                  draggedHandle = None
                } else
                  updateChar
              }

          }
      }
    } else
      standardPicking

    def standardPicking = curentCharacter {
      c =>

        val local = componentCoord((evt.pageX, evt.pageY), charContext.canvasElem)
        val index = Picker.pick(local, c, charContext)
        if (index >= 0) {

          val adress = charMaker.locationMap(c.parts(index).imageId)
          if (selection.layer >= 0 || adress.part != selection.part || adress.category != selection.category) {
            selection = adress
          }

        }

        def ifLayer(l: CMLayer) = {
          val tr = l.transform
          distHeldMouse = stat.partInvertTransform * local - (tr.dx, tr.dy)
          partHeld = true
          selectedShape match {
            case None =>
            case Some((cat, p, s)) =>
              l match {
                case _: CMShape =>
                  if (cat == selection.category &&
                    p == selection.part &&
                    s == selection.layer &&
                    selection.layerSelect == SelectShapes) {
                    selectedCurveComand = -1
                    drawChar(c)
                  }
                case _ =>
              }
          }

        }
        def ifPart(p: CMPart) = {
          val tr = p.partTransform
          distHeldMouse = stat.globalInvertTransform * local - (tr.dx, tr.dy)
          partHeld = true
        }
        selection.forSelected(charMaker, ifLayer _, ifPart _, (c) => Unit)
        ParMenuDrawer.update(setting, this)

    }
  }
  def onMouseWheel(evt: WheelEvent): Any = {
    if ((shiftIsDown || !devMod) && !dragAll) {
      val wheel = evt.deltaY * wheelZoomSpeed
      val tr = globalTransform
      val zoom = math.exp(wheel) * tr.sx
      globalTransform = Transforme(zoom, zoom, 0, tr.dx, tr.dy)
      updateChar
    }
  }
  def onMouseMove(evt: MouseEvent): Any = {
    def f(p: CMLayer) = {
      val tr = p.transform
      val mousePos = componentCoord((evt.pageX, evt.pageY), charContext.canvasElem)
      val pos = stat.partInvertTransform * mousePos - distHeldMouse
      val newTr = Transforme(tr.sx, tr.sy, tr.rotation, pos.x, pos.y)
      p.setTransform(newTr)
    }
    def g(p: CMPart) = {
      val tr = p.partTransform
      val mousePos = componentCoord((evt.pageX, evt.pageY), charContext.canvasElem)
      val pos = stat.globalInvertTransform * mousePos - distHeldMouse
      val newTr = Transforme(tr.sx, tr.sy, tr.rotation, pos.x, pos.y)
      p.setTransform(newTr)
    }

    if (dragAll) {
      val mousePos = componentCoord((evt.pageX, evt.pageY), charContext.canvasElem)
      val tr = globalTransform
      val pos = mousePos - distHeldMouse * (tr.sx, tr.sy)
      globalTransform_=(Transforme(tr.sx, tr.sy, tr.rotation, pos.x, pos.y))
      updateChar
    } else if (draggedHandle.isDefined) {
      val (c, p, l) = selectedShape.get

      val part = charMaker.getPart(c, p)
      val shape = part.shapes(l)
      ShapeManipulator.move(
        componentCoord((evt.pageX, evt.pageY), charContext.canvasElem),
        draggedHandle.get,
        shape,
        this,
        stat.layerInvertTransform)
    } else if (partHeld) {
      {
        val adress = selection
        charMaker =
          charMaker.updated(adress, f _, g, (c) => throw new UnsupportedOperationException("Can't drag categories"))

        updateChar
        ParMenuDrawer.update(setting, this)
      }
    }

  }
  def onMouseUp(evt: MouseEvent): Any = {
    partHeld = false
    dragAll = false
    draggedHandle = None
  }
  def onColorChange(color: Int, jscolor: JQueryEventObject): Any = {
    colorMask = colorMask.updated(color, "#" + jscolor.target.asInstanceOf[Dynamic].value.toString)
    resetSliders
    updateChar
    if (selection._3 >= 0)
      ParMenuDrawer.update(setting, this)
  }
  def onShapeAlphaColorChange(newAlpha: Float, colorIndex: Int) = {
    def f(shape: CMShape) = {
      val col = shape.colors(colorIndex).setAlpha(newAlpha)
      shape.setColors(shape.colors.updated(colorIndex, col))
    }
    if (selection._3 < 0)
      throw new Exception("Part nor condition have constant colors")
    val CMAdress(cat, opt, ref, _) = selection
    charMaker = charMaker.updateShape(cat, opt, ref, f _)
    updateChar
  }
  def onShapeCstColorChange(newVal: String, colorIndex: Int) = {
    def f(shape: CMShape) = {
      shape.colors(colorIndex) match {
        case cst: ConstantColor =>
          shape.setColors(shape.colors.updated(colorIndex, cst.setConstantColor(newVal)))
        case other =>
          throw new UnsupportedOperationException("Can't set const color on" + other)
      }
    }
    if (selection._3 < 0)
      throw new Exception("Part nor condition have constant colors")
    val CMAdress(cat, opt, ref, _) = selection
    charMaker = charMaker.updateShape(cat, opt, ref, f _)
    updateChar
  }
  def onShapeManipulated(newHandlePos: Vec, draggedCurve: Int, draggedPoint: Int) = {
    selectedShape match {
      case None =>
      case Some((cat, part, shap)) =>
        val s = charMaker.getPart(cat, part).shapes(shap)
        val newCurve = s.commands(draggedCurve).update(draggedPoint, newHandlePos)
        val newShape = s.setDrawCommand(newCurve, draggedCurve)
        charMaker = charMaker.updated(s, (l: CMLayer) => newShape)
    }
    updateChar
  }
  def onPartInMenuClicked(category: String, part: String, evt: JQueryEventObject): Any = {

    val catIndex = charMaker.categories.indexWhere { _.categoryName == category }
    val partIndex = charMaker.categories(catIndex).possibleParts.indexWhere { _.partName == part }
    if (partIndex < 0 || choices(catIndex) == partIndex) {
      selection = CMAdress(catIndex, partIndex)
      ParMenuDrawer.update(setting, this)
    } else {
      val oldChoices = choices
      choices = choices.updated(catIndex, partIndex)
      onMenuChanged(charMaker, charMaker, oldChoices, choices)
      updateChar
      /*if (catIndex < 0 || catIndex == selection.category && partIndex != selection.part) {
        selection = (catIndex, partIndex, -1)
        ParMenuDrawer.update(setting, this)
      }*/
    }

  }

  def onCategoryCreated(newCatName: String) = {
    val okCatName = {
      val trimedCat = newCatName.trim()
      val startCatName = if (trimedCat == "") {
        "Anon"
      } else {
        trimedCat
      }
      def isOk(name: String) = !charMaker.categories.exists { _.categoryName == name }
      def reqNameCat(i: Int = 0): String = {
        val testCatName = startCatName + "(" + i + ")"
        if (isOk(testCatName))
          testCatName
        else
          reqNameCat(i + 1)
      }
      if (isOk(startCatName))
        startCatName
      else {
        reqNameCat()
      }
    }
    val noImage = new CMImage("None", Transforme(), "None", 0, AlwayVisible, "Nothing")
    val emptyPart = new CMPart("Empty", Seq(noImage), Nil, Transforme(), 0, CMPart.newLinkKey)

    val oldCm = charMaker
    charMaker = charMaker.addPart(okCatName, emptyPart)
    val (c, p) = charMaker.getLocation(okCatName, "Empty")
    selection = CMAdress(c, p)
    updateAll(oldCm, choices)

  }
  def onManyLayersImported(refs: Seq[String]) = {
    def simpleRef(s: String) = s.split("images[/\\\\]").last
    val CMAdress(c, p, _, _) = selection
    def f(part: CMPart) = {
      def newComp(s: String) =
        new CMImage(simpleRef(s), Transforme(), "None", 0, AlwayVisible, simpleRef(s))
      val newPart = part.setImages((part.images ++ refs.map(newComp)).sortBy { _.ref })
      charMaker = charMaker.updated(c, p, newPart)
    }
    def g(cat: CMCategory) = {

      val newParts = refs
        .map(s => new CMImage(simpleRef(s), Transforme(), "None", 0, AlwayVisible, simpleRef(s)))
        .map(i => new CMPart(i.ref, Seq(i), Nil, Transforme(), 0, CMPart.newLinkKey)) ++
        cat.possibleParts
      val newCat = new CMCategory(cat.categoryName, newParts.sortBy(_.partName))
      charMaker = charMaker.updated(c, newCat)
    }

    val oldCM = charMaker
    val oldChoices = choices
    selection.forSelected(charMaker, (l: CMLayer) => Unit, f _, g _)

    updateAll(oldCM, oldChoices)
  }
  def onImageRelocated(newCategory: String, newPart: String) = {

    def relocateLayer(l: CMLayer) = {
      val newCm = charMaker.remove(selection).add(newCategory, newPart, l)
      selection = newCm.getLocation(l)
      charMaker = newCm.enforceLinkConsitancy

    }
    def relocatePart(oldPart: CMPart) = {

      val relocated = oldPart.setName(newPart)

      val newCm = charMaker
        .remove(selection.category, selection.part).addPart(newCategory, relocated)
        .enforceLinkConsitancy
      val (c, p) = newCm.getLocation(newCategory, relocated.partName)
      selection = CMAdress(c, p)
      charMaker = newCm
    }
    def relocateCategory(oldCat: CMCategory) = {
      charMaker = charMaker.updated(selection.category, (c: CMCategory) => c.setName(newCategory))
        .enforceLinkConsitancy
    }
    val oldCM = charMaker

    val oldChoices = choices
    selection.forSelected(
      charMaker,
      relocateLayer _,
      relocatePart _,
      relocateCategory _)

    if (charMaker != oldCM) {
      if (selection.category < choices.size) {
        choices = choices.updated(selection.category, selection.part)
      }

      updateAll(oldCM, oldChoices)
    }
  }
  def onImageCopyed() = {
    val CMAdress(category, part, imageindex, _) = selection
    val oldCm = charMaker
    def makeUnusedName(candiate: String, usedNames: Seq[String]) = {
      def isok(s: String) = !usedNames.contains(s)
      def reqNewName(index: Int = 0): String = {
        val candidatei = candiate + "(" + index + ")"
        if (isok(candidatei))
          candidatei
        else
          reqNewName(index + 1)
      }

      if (isok(candiate))
        candiate
      else
        reqNewName()
    }
    val oldChoices = choices
    def copyCat(selectedCategory: CMCategory) = {

      val usedNames = charMaker.categories.map { _.categoryName }
      val newName = makeUnusedName(selectedCategory.categoryName + "-copy", usedNames)
      val newCategory = selectedCategory.setName(newName)
      charMaker = charMaker.add(newCategory)
      selection = CMAdress(category)

    }
    def copyPart(selectedPart: CMPart) = {

      val usedNames = charMaker.categories(category).possibleParts.map(_.partName)
      val newName = makeUnusedName(selectedPart.partName + "-copy", usedNames)
      val newPart = selectedPart.setName(newName).removeLink
      charMaker = charMaker.add(category, newPart)
      selection = CMAdress(category, charMaker.categories(category).possibleParts.indexOf(newPart))
      choices = choices.updated(category, part)
    }
    def copyLayer(layer: CMLayer) = {
      val newLayer = layer.changeId
      charMaker = charMaker.add(category, part, newLayer)
      choices = choices.updated(category, part)
      selection = charMaker.locationMap(newLayer.id)
    }
    selection.forSelected(charMaker, copyLayer(_), copyPart(_), copyCat(_))
    updateAll(oldCm, oldChoices)
  }
  def onPartCreated(partName: String) = {
    val hostCat = charMaker.categories(selection.category)
    val okPartName = {
      val trimedPart = partName.trim()
      val startPartName = if (trimedPart == "") {
        "Anon"
      } else {
        trimedPart
      }
      def isOk(name: String) = !hostCat.possibleParts.exists { _.partName == name }
      def reqNamePart(i: Int = 0): String = {
        val testPartName = startPartName + "(" + i + ")"
        if (isOk(testPartName))
          testPartName
        else
          reqNamePart(i + 1)
      }
      if (isOk(startPartName))
        startPartName
      else {
        reqNamePart()
      }
    }

    val i = new CMImage("None", Transforme(), "None", 0, AlwayVisible, "Nothing")
    val p = new CMPart(okPartName, Seq(i), Nil, Transforme(), 0, CMPart.newLinkKey)
    val oldCM = charMaker
    charMaker = charMaker.add(selection.category, p)
    updateAll(oldCM, choices)
  }
  def onShapeCreated = {
    val shape = new CMShape(
      Seq(new MoveTo(50, 50),
        new CurveTo((50, 50), (50, 100), (50, 100)),
        new CurveTo((50, 100), (100, 100), (100, 100)),
        new CurveTo((100, 100), (100, 50), (100, 50))),
      Transforme(),
      Seq(ConstantColor("black", 1), ConstantColor("white", 0.5f)),
      0,
      AlwayVisible,
      5,
      true,
      "miter",
      true,
      Nil,
      "A Nameless Curve")
    val oldCM = charMaker
    charMaker = charMaker.add(selection.category, selection.part, shape)
    updateAll(oldCM, choices)
  }
  def onImageCreated = {
    val image = new CMImage("None", Transforme(), "None", 0, AlwayVisible, "Nothing")
    val oldCM = charMaker
    charMaker = charMaker.add(selection.category, selection.part, image)
    updateAll(oldCM, choices)
  }
  def onImageCreated(image: CMImage) = {
    val CMAdress(category, part, _, _) = selection
    val oldCm = charMaker
    charMaker = charMaker.add(category, part, image)
    val oldChoices = choices
    choices = choices.updated(category, part)
    selection = CMAdress(
      category,
      part,
      charMaker.categories(category).possibleParts(part).images.indexOf(image),
      SelectImages)
    updateAll(oldCm, oldChoices)
  }
  def onImageDeleted = {
    val oldCM = charMaker
    val CMAdress(category, part, _, _) = selection

    def deletCat(c: CMCategory) {
      charMaker = charMaker.remove(category).enforceLinkConsitancy
    }
    def deletPart(p: CMPart) {
      charMaker = charMaker.remove(category, part).enforceLinkConsitancy
    }
    def deletLayer(l: CMLayer) {
      charMaker = charMaker.remove(selection).enforceLinkConsitancy
    }
    selection.forSelected(charMaker, deletLayer, deletPart, deletCat)
    if (charMaker != oldCM)
      updateAll(oldCM, choices)
  }
  def onImageTransformed(movement: (Transforme, Float) => (Transforme, Float)) = {
    val CMAdress(category, part, image, _) = selection
    def f(p: CMLayer) = {

      val (newTransform, newZ) = movement(p.transform, p.z)
      p.setZ(newZ).setTransform(newTransform)
    }
    def g(p: CMPart) = {
      val (newTransform, newZ) = movement(p.partTransform, p.partZ)
      p.setZ(newZ).setTransform(newTransform)
    }
    def h(c: CMCategory) = {
      c.setPart(c.possibleParts.map(g))
    }
    def trCategory(c: CMCategory) = {
      charMaker = charMaker.updated(category, h _)
    }
    def trPart(p: CMPart) = {
      charMaker = charMaker.updated(category, part, g _)
    }
    def trLayer(l: CMLayer) = {
      charMaker = charMaker.updated(selection, f _)
    }
    selection.forSelected(charMaker, trLayer, trPart, trCategory)
    updateChar
  }
  def onImageBoundColorChange(newBoundColor: String, colorIndex: Int) = {
    val CMAdress(category, part, image, _) = selection
    def f(cmi: CMLayer) = {
      cmi match {
        case img: CMImage =>
          img.setColorBond(newBoundColor)
        case shape: CMShape =>
          val col = shape.colors(colorIndex)
          shape.setColors(shape.colors.updated(colorIndex, DynamicColor(newBoundColor).setAlpha(col.alpha)))
      }

    }
    def setImageColor(cmi: CMImage) = {
      cmi.setColorBond(newBoundColor)
    }
    def setShapeColor(shape: CMShape) = {
      val col = shape.colors(colorIndex)
      shape.setColors(shape.colors.updated(colorIndex, DynamicColor(newBoundColor).setAlpha(col.alpha)))

    }
    def setPartColor(p: CMPart) = {
      val newIms = p.images.map(setImageColor)
      val newShapes = p.shapes.map(setShapeColor)
      p.setImages(newIms).setShapes(newShapes)

    }
    def setCatColor(c: CMCategory) = {
      c.setPart(c.possibleParts.map(setPartColor))
    }
    val oldCM = charMaker

    charMaker = charMaker.updated(selection, setImageColor, setShapeColor, setPartColor, setCatColor)

    updateAll(oldCM, choices)
  }
  def onLayerChanged(change: (CMLayer) => CMLayer) {
    val oldCM = charMaker
    charMaker = charMaker.updated(selection, change)
    updateAll(oldCM, choices)
  }
  def onImageRefChanged(newImageRef: String) = {
    val oldCM = charMaker
    selection.forSelectedImage(charMaker) {
      cmi =>
        charMaker = charMaker.updated(selection, cmi.setRef(newImageRef))
    }
    updateAll(oldCM, choices)
  }
  def askNameChange(newName: String): String = {
    def blabla(level: String): String = {
      "There already is a " + level +
        " named " + newName + ".\nYou can use the location pannel if you want to merge this " +
        level + " with an other."
    }
    selection.getCategory(charMaker) match {
      case None => "Nothing is selected.\n I think Nothing is already a good name for nothing."
      case Some(c) =>
        selection.getPart(charMaker) match {
          case None =>
            if (charMaker.categories.exists { _.categoryName == newName })
              blabla("category")
            else {
              val newCats = charMaker.categories
                .updated(selection.category, c.setName(newName))
                .sortBy { _.categoryName }
              val i = newCats.indexWhere { _.categoryName == newName }
              val oldCM = charMaker
              val oldChoices = choices
              val unchangedChoices = choices.take(selection.category) ++ choices.drop(selection.category + 1)
              choices = (choices.take(i) :+ selection.part) ++ choices.drop(i)
              selection = CMAdress(i, selection.part, selection.layer, selection.layerSelect)
              charMaker = new CharMaker(newCats, charMaker.colors, charMaker.sliders, charMaker.imageMap)
              updateAll(oldCM, oldChoices)
              ""
            }
          case Some(p) =>

            selection.getLayer(charMaker) match {
              case None => if (c.possibleParts.exists { _.partName == newName })
                blabla(" part in " + c.categoryName)
              else {
                val newParts = c.possibleParts
                  .updated(selection.part, p.setName(newName))
                  .sortBy { _.partName }
                val i = newParts.indexWhere { _.partName == newName }
                val oldCM = charMaker
                val oldChoices = choices
                choices = choices.updated(selection.part, i)
                charMaker = charMaker.updated(selection.category, c.setPart(newParts))
                updateAll(oldCM, oldChoices)
                ""

              }
              case Some(l) =>
                val oldCM = charMaker
                l match {
                  case img: CMImage =>
                    if (p.images.exists(_.name == newName))
                      blabla(" image in " + p.partName + " in " + c.categoryName)
                    else {
                      charMaker = charMaker.updated(selection, img.setName(newName))
                      updateAll(oldCM, choices)
                      ""
                    }
                  case s: CMShape =>
                    if (p.shapes.exists(_.name == newName))
                      blabla(" shape in " + p.partName + " in " + c.categoryName)
                    else {
                      charMaker = charMaker.updated(selection, s.setName(newName))
                      updateAll(oldCM, choices)
                      ""
                    }
                }

            }
        }
    }
  }
  def selectPart {
    selection = CMAdress(selection.category, selection.part)
    updateAll(charMaker, choices)
  }
  def selectCategrory {
    selection = CMAdress(selection.category)
    updateAll(charMaker, choices)
  }
  def onImageConditionChanged(newCondtion: VisibilityCondition) = {
    val oldCM = charMaker
    selection.forWhateverSelected(charMaker) {
      layer =>
        charMaker = charMaker.updated(selection, layer.setCondition(newCondtion))
    }
    updateAll(oldCM, choices)
  }
  def setShapeSelected(selected: Boolean) {
    if (selected) {
      selection.forSelectedShape(charMaker) {
        s =>

          selectedCurveComand =
            charMaker.getShape(selection.category, selection.part, selection.layer).commands.size - 1
          selectedShape = Some((selection.category, selection.part, selection.layer))
          resetSliders
      }
    } else {
      selectedCurveComand = -1
      selectedShape = None
    }

    updateChar
  }
  def onShapeLoosingComande(category: Int, part: Int, shape: Int, curve: Int) {
    val oldCM = charMaker
    val targetShape = charMaker.getPart(category, part).shapes(shape)
    val newShape = ShapeManipulator.removeCommande(targetShape, curve)
    charMaker = charMaker.updateShape(category, part, shape, newShape)
    updateAll(oldCM, choices)
  }
  def onShapeRecivingNewCommand(commandPos: Vec, category: Int, part: Int, shape: Int) {
    val oldCM = charMaker
    val targetShape = charMaker.getPart(category, part).shapes(shape)
    val (newShape, newSelect) = ShapeManipulator.addCommande(targetShape, selectedCurveComand, commandPos)
    selectedCurveComand = newSelect
    charMaker = charMaker.updateShape(category, part, shape, newShape)
    updateAll(oldCM, choices)
  }
  def onLayerSelected(part: Int, image: Int, select: LayersSelector) = {
    selection = CMAdress(selection.category, part, image, select)
    ParMenuDrawer.update(setting, this)
  }
  def onLayerSelected(selected: Int, select: LayersSelector) = {
    selection = CMAdress(selection.category, selection.part, selected, select)
    ParMenuDrawer.update(setting, this)
  }
  def onPartSelected(selected: Int) = {
    selection = CMAdress(selection.category, selected)
    ParMenuDrawer.update(setting, this)
  }
  def onCookieSave(evt: JQueryEventObject) = {
    CMPrinter.cookie(stat)
  }
  def onSave(evt: JQueryEventObject) = {
    CMPrinter.print(setting.saveFileName, stat)
  }
  def onSliderChange(sliderIndex: Int, newValues: Int) = {

    slidersValues = slidersValues.updated(sliderIndex, newValues)
    updateChar
  }
  def onMenuChanged(oldCm: CharMaker, newCM: CharMaker, oldChoices: Seq[Int], newChoices: Seq[Int]) = {
    DrawMenu.updateMenu(oldCm, newCM, oldChoices, newChoices, this, setting)
  }
  def componentCoord(v: Vec, local: HTMLElement) = {
    @tailrec
    def rec(elem: HTMLElement, acc: Vec = (0.0, 0.0)): Vec = {
      if (elem != null) {
        val offset = acc + (elem.offsetLeft, elem.offsetTop)
        elem.offsetParent match {
          case parent: HTMLElement => rec(parent, offset)
          case _                   => offset
        }

      } else
        acc

    }
    val offset: Vec = rec(local)
    v - offset
  }
  def resetSliders = {
    if (slidersValues.exists(_ != 0)) {
      slidersValues = Seq.fill(slidersValues.size)(0)

      SlidersMenu.update(this, setting)
    }

  }

}