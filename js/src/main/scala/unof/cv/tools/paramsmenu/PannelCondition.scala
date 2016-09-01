package unof.cv.tools.paramsmenu

import scala.scalajs.js
import scala.scalajs.js.Any.fromFunction1
import scala.scalajs.js.Any.fromInt
import org.scalajs.jquery.JQuery
import org.scalajs.jquery.JQueryEventObject
import org.scalajs.jquery.jQuery
import unof.cv.base.charmaker.AlwayVisible
import unof.cv.base.charmaker.CMAdress.toT4
import unof.cv.base.charmaker.LinkedVisibility
import unof.cv.base.charmaker.VisibilityCondition
import unof.cv.base.charmaker.VisibleIfNoLink
import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting
import unof.cv.tools.CvSetting

object PannelCondition extends SharedPannelFunctions with LayerTypeInsensitvePannel with BasicPannel {
  
  def ifCategorySelected(callbacks: CallbackCenter, settings: CvSetting, cat: unof.cv.base.charmaker.CMCategory): Unit = {
    hide(settings)
  }
  def ifLayerSelected(callbacks: CallbackCenter, settings: CvSetting, image: unof.cv.base.charmaker.CMLayer): Unit = {
    show(settings)
    val typeInput = jQuery(settings.conditionTypeSelect)
    val condition = image.displayCondition
    val catInput = jQuery(settings.catBindingInput)
    val partInput = jQuery(settings.partBindingInput)
    typeInput.value(conditionName(condition))
    condition match {
      case LinkedVisibility(key) =>
        activateCatAndPartBondSelect(
          callbacks,
          jQuery(settings.boundableCatList),
          catInput,
          jQuery(settings.boundablePartList),
          partInput)
        val (catName, partName) = callbacks.charMaker.linkKeyMap(key)
        catInput.value("" + catName)
        catSelectBondChange(callbacks, catInput, partInput, jQuery(settings.boundablePartList))(null)
        partInput.value("" + partName)
      case _ =>
        catInput.hide(500)
        partInput.hide(500)

    }

  }
  def ifPartSelected(callbacks: CallbackCenter, settings: CvSetting, part: unof.cv.base.charmaker.CMPart): Unit = {
    hide(settings)
  }
  def myPannel(s: CvSetting) = s.conditionsDiv

  def bind(callbacks: CallbackCenter, settings: CvSetting) {
    val typeInput = jQuery(settings.conditionTypeSelect)
    val catBondList = jQuery(settings.boundableCatList)
    val partBondList = jQuery(settings.boundablePartList)
    val catSelect = jQuery(settings.catBindingInput)
    val partSelect = jQuery(settings.partBindingInput)
    val typesName = Seq(
      conditionName(AlwayVisible),
      conditionName(LinkedVisibility(0)),
      conditionName(VisibleIfNoLink))
    setOptionsInList(typesName, typeInput)
    typeInput.change(conditionTypeChange(callbacks, typeInput, catBondList, catSelect, partBondList, partSelect)_)
    catSelect.change(catSelectBondChange(callbacks, catSelect, partSelect, partBondList)_)

    val conditionButton = jQuery(settings.validateCondtion)
    conditionButton.click(onVisibilityConditionChange(callbacks, typeInput, catSelect, partSelect) _)

  }
  def conditionName(c: VisibilityCondition) = c match {
    case LinkedVisibility(_) => "Linked to"
    case AlwayVisible        => "Always"
    case VisibleIfNoLink     => "No visible links"
  }
  private def activateCatAndPartBondSelect(
      callbacks: CallbackCenter,
      catList: JQuery,
      catInput: JQuery,
      partList: JQuery,
      partInput: JQuery
  ) {
    catInput.show(500)
    val forbiddenCatIndex = callbacks.selection._1
    val allCats = callbacks.currentOptions.categories
    val allowedCats = allCats.take(forbiddenCatIndex) ++ allCats.drop(forbiddenCatIndex + 1)
    setOptionsInList(allowedCats.map(_.categoryName), catList)
    val topCat = allowedCats.head
    setOptionsInList(topCat.possibleParts.map(_.partName), partList)
    partInput.show(500)
  }
  private def catSelectBondChange(callbacks: CallbackCenter, catInput: JQuery, partInput: JQuery, partData: JQuery)(evt: JQueryEventObject) {
    val catName = catInput.value().toString()
    val partNames = callbacks.currentOptions.getCategory(catName).possibleParts.map(_.partName)
    setOptionsInList(partNames, partData)
  }
  private def onVisibilityConditionChange(callbacks: CallbackCenter, typeInput: JQuery, catInput: JQuery, partInput: JQuery)(evt: JQueryEventObject) = {
    val conditionType = typeInput.value().toString()
    if (conditionType == conditionName(AlwayVisible)) {
      callbacks.onImageConditionChanged(AlwayVisible)
    } else if (conditionType == conditionName(VisibleIfNoLink)) {
      callbacks.onImageConditionChanged(VisibleIfNoLink)
    } else if (conditionType == conditionName(LinkedVisibility(0))) {
      val category = catInput.value().toString()
      val part = partInput.value().toString()
      val linkedPart = callbacks.currentOptions.getPart(category, part)
      callbacks.onImageConditionChanged(LinkedVisibility(linkedPart.linkKey))
    } else {
      throw new NotImplementedError("WTF is the condition " + conditionType + "?")
    }

  }
  private def conditionTypeChange(callbacks: CallbackCenter, typeInput: JQuery, catList: JQuery, catInput: JQuery, partList: JQuery, partInput: JQuery)(evt: JQueryEventObject) {
    val inputTypeName = typeInput.value().toString()
    if (inputTypeName == conditionName(AlwayVisible) || inputTypeName == conditionName(VisibleIfNoLink)) {
      catInput.hide(500)
      partInput.hide(500)
    } else {
      activateCatAndPartBondSelect(callbacks, catList, catInput, partList, partInput)
    }
  }
}