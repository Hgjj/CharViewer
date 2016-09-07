package unof.cv.tools.paramsmenu

import scala.scalajs.js
import scala.scalajs.js.Any.fromFunction1
import scala.scalajs.js.Any.fromInt
import org.scalajs.jquery.JQuery
import org.scalajs.jquery.JQueryEventObject
import org.scalajs.jquery.jQuery
import unof.cv.base.charLib.AlwayVisible
import unof.cv.base.charLib.CMAdress.toT4
import unof.cv.base.charLib.LinkedVisibility
import unof.cv.base.charLib.VisibilityCondition
import unof.cv.base.charLib.VisibleIfNoLink
import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting
import unof.cv.tools.CvSetting
import unof.cv.base.charLib.SliderVisibility
import SharedPannelFunctions._

object PannelCondition extends  LayerTypeInsensitvePannel with BasicPannel {

  def ifCategorySelected(callbacks: CallbackCenter, settings: CvSetting, cat: unof.cv.base.charLib.CMCategory): Unit = {
    hide(settings)
  }
  def ifLayerSelected(callbacks: CallbackCenter, settings: CvSetting, image: unof.cv.base.charLib.CMLayer): Unit = {
    show(settings)
    val typeInput = jQuery(settings.conditionTypeSelect)
    val condition = image.displayCondition
    val catInput = jQuery(settings.catBindingInput)
    val partInput = jQuery(settings.partBindingInput)
    val sliderInput = jQuery(settings.sliderBindingInput)
    val oppInput = jQuery(settings.oppSelect)
    val ceilInput = jQuery(settings.conditionCeil)
    typeInput.value(condition.key)
    condition match {
      case LinkedVisibility(key) =>
        sliderInput.hide(500)
        oppInput.hide(500)
        ceilInput.hide(500)
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
      case SliderVisibility(slider, opp, value) =>
        sliderInput.show(500)
        oppInput.show(500)
        ceilInput.show(500)
        catInput.hide(500)
        partInput.hide(500)
        sliderInput.value(slider)
        setOptionsInList(callbacks.charMaker.sliders, sliderInput)
        oppInput.value(SliderVisibility.parseOpp(opp))
        ceilInput.value("" + value)

      case _ =>
        sliderInput.hide(500)
        oppInput.hide(500)
        ceilInput.hide(500)
        catInput.hide(500)
        partInput.hide(500)

    }

  }
  def ifPartSelected(callbacks: CallbackCenter, settings: CvSetting, part: unof.cv.base.charLib.CMPart): Unit = {
    hide(settings)
  }
  def myPannel(s: CvSetting) = s.conditionsDiv

  def bind(callbacks: CallbackCenter, settings: CvSetting) {
    val typeInput = jQuery(settings.conditionTypeSelect)
    val catBondList = jQuery(settings.boundableCatList)
    val partBondList = jQuery(settings.boundablePartList)
    val catSelect = jQuery(settings.catBindingInput)
    val partSelect = jQuery(settings.partBindingInput)
    val slides = jQuery(settings.sliderBindingInput)
    val opps = jQuery(settings.oppSelect)
    val ceils = jQuery(settings.conditionCeil)
    val typesName = Seq(
      AlwayVisible.key,
      LinkedVisibility.key,
      VisibleIfNoLink.key,
      SliderVisibility.key)
    setOptionsInList(typesName, typeInput)
    typeInput.change(conditionTypeChange(
      callbacks,
      typeInput,
      catBondList,
      catSelect,
      partBondList,
      partSelect,
      slides,
      opps,
      ceils)_)
    catSelect.change(catSelectBondChange(callbacks, catSelect, partSelect, partBondList)_)

    val conditionButton = jQuery(settings.validateCondtion)
    conditionButton.click(onVisibilityConditionChange(
      callbacks,
      typeInput,
      catSelect,
      partSelect,
      slides,
      opps,
      ceils) _)

    val oppInput = jQuery(settings.oppSelect)
    setOptionsInList(Seq("<", ">", "<=", ">=", "==", "!="), oppInput)
  }

  private def activateCatAndPartBondSelect(
    callbacks: CallbackCenter,
    catList: JQuery,
    catInput: JQuery,
    partList: JQuery,
    partInput: JQuery) {
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
  private def onVisibilityConditionChange(
    callbacks: CallbackCenter,
    typeInput: JQuery,
    catInput: JQuery,
    partInput: JQuery,
    sliderInput: JQuery,
    oppInput: JQuery,
    ceilInput: JQuery)(evt: JQueryEventObject) = {

    val conditionType = typeInput.value().toString()
    if (conditionType == AlwayVisible.key) {
      callbacks.onImageConditionChanged(AlwayVisible)
    } else if (conditionType == VisibleIfNoLink.key) {
      callbacks.onImageConditionChanged(VisibleIfNoLink)
    } else if (conditionType == LinkedVisibility.key) {
      val category = catInput.value().toString()
      val part = partInput.value().toString()
      val linkedPart = callbacks.currentOptions.getPart(category, part)
      callbacks.onImageConditionChanged(LinkedVisibility(linkedPart.linkKey))
    } else if (conditionType == SliderVisibility.key) {
      val opp = SliderVisibility.parseOpp(oppInput.value().toString())
      val ceil = ceilInput.value().toString().toInt
      val linkedSlider = sliderInput.value().toString()
      callbacks.onImageConditionChanged(SliderVisibility(linkedSlider, opp, ceil))
    } else {
      throw new NotImplementedError("WTF is the condition " + conditionType + "?")
    }

  }
  private def conditionTypeChange(
    callbacks: CallbackCenter,
    typeInput: JQuery,
    catList: JQuery,
    catInput: JQuery,
    partList: JQuery,
    partInput: JQuery,
    sliderInput: JQuery,
    oppInput: JQuery,
    ceilInput: JQuery)(evt: JQueryEventObject) {
    val inputTypeName = typeInput.value().toString()
    if (inputTypeName == AlwayVisible.key || inputTypeName == VisibleIfNoLink.key) {
      catInput.hide(500)
      partInput.hide(500)
      sliderInput.hide(500)
      oppInput.hide(500)
      ceilInput.hide(500)
    } else if (inputTypeName == LinkedVisibility.key) {
      sliderInput.hide(500)
      oppInput.hide(500)
      ceilInput.hide(500)
      activateCatAndPartBondSelect(callbacks, catList, catInput, partList, partInput)
    } else if (inputTypeName == SliderVisibility.key) {
      sliderInput.show(500)
      oppInput.show(500)
      ceilInput.show(500)
      catInput.hide(500)
      partInput.hide(500)
      setOptionsInList(callbacks.charMaker.sliders, sliderInput)
    } else ???
  }
}