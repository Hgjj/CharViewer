package unof.cv.tools

import unof.cv.base.charLib.CharacterLibrary
import unof.cv.utils.Transforme
import unof.cv.base.charLib.CMShape
import unof.cv.base.charLib.LayersSelector
import unof.cv.base.charLib.LayersSelector
import unof.cv.base.charLib.LayersSelector
import unof.cv.base.charLib.CMAdress

class AppStat(
    private var myChoices: Seq[Int],
    private var myColorMask: Seq[String],
    private var mySlidersValues : Seq[Int],
    private var mySelection: CMAdress,
    private var myCharMaker: CharacterLibrary,
    private var myGlobalTransform: Transforme,
    private var mySelectedShape: Option[(Int, Int, Int)]) {

  private var partInvTr = Transforme()
  private var layerInvTr = Transforme()
  private var globInvTr = myGlobalTransform.invert

  updatInvTransform

  private val undoRedo = new UndoRedo(200, 200, 100)

  def lastEddition = undoRedo.topDiffDate

  def forgetPast = undoRedo.reset
  def choices = myChoices
  def choices_=(c: Seq[Int]) = {
    undoRedo.makeDif(ChoiceDiff(myChoices), ChoiceDiff(c))
    myChoices = c
  }
  def colorMask = myColorMask
  def colorMask_=(c: Seq[String]) = {
    undoRedo.makeDif(ColorsDiff(myColorMask), ColorsDiff(c))
    myColorMask = c
  }
  def selection = mySelection
  def selection_=(c: CMAdress) = {
    undoRedo.makeDif(mySelection, c)
    mySelection = c
    updatInvTransform
  }
  def charMaker = myCharMaker
  def charMaker_=(c: CharacterLibrary) = {
    undoRedo.makeDif(myCharMaker, c)
    myCharMaker = c
  }
  def globalTransform_=(t: Transforme) = {
    undoRedo.makeDif(myGlobalTransform, t)
    myGlobalTransform = t
    globInvTr = myGlobalTransform.invert
    updatInvTransform
  }
  def selectedShape = mySelectedShape
  def selectedShape_=(s: Option[(Int, Int, Int)]) = {
    undoRedo.makeDif(mySelectedShape, s)
    mySelectedShape = s
  }
  def slidersValues = mySlidersValues
  def slidersValues_=(s: Seq[Int]) = {
    undoRedo.makeDif(SliderDiff(mySlidersValues), SliderDiff(s))
    mySlidersValues = s
  }
  def globalTransform = myGlobalTransform
  def partInvertTransform = partInvTr
  def layerInvertTransform = layerInvTr
  def globalInvertTransform = globInvTr
  case class ChoiceDiff(choices: Seq[Int])
  case class ColorsDiff(colors: Seq[String])
  case class SliderDiff(sliders : Seq[Int])
  private def applyDif(diff: Option[Any]): Unit = {
    def apllyEffectiveDif(effDif: Any): Unit = effDif match {
      case ChoiceDiff(choice) =>
        myChoices = choice
      case ColorsDiff(colors) =>
        myColorMask = colors
      case selection: CMAdress =>
        mySelection = selection
        updatInvTransform
      case cm: CharacterLibrary =>
        myCharMaker = cm
      case tr: Transforme =>
        myGlobalTransform = tr
        globInvTr = myGlobalTransform.invert
        updatInvTransform
      case  Some((c:Int, p:Int, s:Int)) =>
        mySelectedShape = Some((c,p,s))
      case None =>
        mySelectedShape = None
      case SliderDiff(sliders)=>
        mySlidersValues = sliders
      case BunchedDiffs(difs) =>
        difs foreach apllyEffectiveDif

    }
    println("App stat apply dif : "+diff)
    diff match {
      case None    =>
      case Some(a) => apllyEffectiveDif(a)

    }
  }
  private def updatInvTransform = {
    if (mySelection.part >= 0 && mySelection.category < charMaker.categories.length) {
      val cat = charMaker
        .categories(mySelection.category)
      if (mySelection.part < cat.possibleParts.size) {
        val part = cat.possibleParts(mySelection.part)
        val partTransform = globalTransform *
          part.partTransform
        partInvTr = partTransform.invert
        if (mySelection.layer >= 0) {
          mySelection.layerSelect.forAnyLayers(part) {
            l => if(mySelection.layer < l.size)
              layerInvTr = (partTransform * l(mySelection.layer).transform).invert
          }
        }
      }

    } else {
      partInvTr = Transforme()
      layerInvTr = Transforme()
    }
  }
  def canUndo = undoRedo.canUndo
  def canRedo = undoRedo.canRedo
  def undo = applyDif(undoRedo.undo)
  def redo = applyDif(undoRedo.redo)
}