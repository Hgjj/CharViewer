package unof.cv.tools.paramsmenu

import org.scalajs.jquery.jQuery
import org.scalajs.jquery.JQuery
import org.scalajs.jquery.JQueryEventObject
import scala.scalajs.js
import scala.scalajs.js.Any.fromFunction1
import scala.scalajs.js.Any.fromInt
import scala.scalajs.js.Any.fromString
import unof.cv.base.charmaker.CMAdress.toT4
import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting
import unof.cv.base.charmaker.CharMaker
import unof.cv.base.charmaker.CMCategory
import unof.cv.base.charmaker.CMLayer
import unof.cv.base.charmaker.CMPart
import unof.cv.base.charmaker.CMAdress

object PannelLocation extends SharedPannelFunctions with LayerTypeInsensitvePannel {
  
  def ifCategorySelected(callbacks: CallbackCenter, settings: CvSetting, cat: CMCategory): Unit = {
    val partInDiv = jQuery(settings.partLocationInputDiv)
    partInDiv.hide(500)
    forAnythingSelected(callbacks, settings)
  }
  def ifLayerSelected(callbacks: CallbackCenter, settings: CvSetting, image: CMLayer): Unit = {
    forAnythingSelected(callbacks, settings)
    val CMAdress(cat,part,_,_) = callbacks.selection
    val partName = callbacks.currentOptions.categories(cat).possibleParts(part).partName
    ifPartIsDefined(callbacks, settings, partName)
  }
  def ifPartSelected(callbacks: CallbackCenter, settings: CvSetting, part: CMPart): Unit = {
    forAnythingSelected(callbacks, settings)
    ifPartIsDefined(callbacks, settings, part.partName)
  }
  private def ifPartIsDefined(callbacks: CallbackCenter, settings: CvSetting, partName: String) {
    val partInDiv = jQuery(settings.partLocationInputDiv)
    partInDiv.show(500)
    val partSpan = jQuery(settings.currentPartSpanId)
    partSpan.empty()
    partSpan.append("<b>" + partName + "</b>")
  }
  private def forAnythingSelected(callbacks: CallbackCenter, settings: CvSetting) = {

    val options = callbacks.charMaker
    val category = callbacks.selection.category

    val catName = options.categories(category).categoryName
    val catSpan = jQuery(settings.currentCatSpanId)
    catSpan.empty()
    catSpan.append("<b>" + catName + "</b>")

    val catInput = jQuery(settings.categorySelectionField)
    val catData = jQuery(settings.categorySuggestionList)
    catInput.value("")
    setOptionsInList(options.categories.map(_.categoryName), catData)
    val partInput = jQuery(settings.partSelectionField)
    partInput.value("")
    val partData = jQuery(settings.partSuggestionLsit)
    setPartSuggestion(options, category, partInput, partData)
  }
  def bind(callbacks: CallbackCenter, settings: CvSetting) {
    val catInput = jQuery(settings.categorySelectionField)

    val partInput = jQuery(settings.partSelectionField)
    val partData = jQuery(settings.partSuggestionLsit)

    catInput.change(updatePartSuggestion(callbacks, catInput, partInput, partData)_)

    val relocButton = jQuery(settings.relocateImageButton)
    relocButton.click(relocateImage(catInput, partInput, callbacks)_)
    
    val catSpan = jQuery(settings.currentCatSpanId)
    catSpan.click(onCatNameClicked(callbacks)_)
    
     val partSpan = jQuery(settings.currentPartSpanId)
     partSpan.click(onPartNameClicked(callbacks)_)
  }
  private def setPartSuggestion(cm: CharMaker, cat: Int, partInput: JQuery, partData: JQuery) = {
    val txt = partInput.value().toString()
    partData.empty()
    if (cat >= 0) {
      setOptionsInList(cm.categories(cat).possibleParts.map(_.partName), partData)
      partInput.append(partData)
    } else {
      partInput.empty()
    }
    partInput.`val`(txt)
  }
  private def updatePartSuggestion(callbacks: CallbackCenter, catInput: JQuery, partInput: JQuery, partData: JQuery)(evt: JQueryEventObject) = {
    val catName = catInput.value().toString()
    val cm = callbacks.currentOptions
    val catIndex = if (catName == "")
      callbacks.currentSelection._1
    else
      cm.categories.indexWhere { _.categoryName == catName }
    setPartSuggestion(cm, catIndex, partInput, partData)
  }
  private def relocateImage(
    catIn: JQuery,
    partIn: JQuery,
    callback: CallbackCenter)(evt: JQueryEventObject) = {
    val cm = callback.currentOptions
    var continu = true
    val catName = catIn.value().toString() match {
      case "" =>
        continu = false
        val category = callback.currentSelection._1
        catIn.value(cm.categories(category).categoryName)
        cm.categories(category).categoryName
      case s => s
    }
    val partName = if (partIn.is(":visible"))
      partIn.value().toString() match {
        case "" =>
          continu = false
          partIn.value("???")
          "???"
        case "???" =>
          continu = false
          ""
        case s => s
      }
    else
      ""
    if (continu) {
      callback.onImageRelocated(catName, partName)
    }
  }
  private def onCatNameClicked(callback : CallbackCenter)(evt : JQueryEventObject)= {
    callback.selectCategrory
  }
  private def onPartNameClicked(callback : CallbackCenter)(evt : JQueryEventObject)= {
    callback.selectPart
  }
}