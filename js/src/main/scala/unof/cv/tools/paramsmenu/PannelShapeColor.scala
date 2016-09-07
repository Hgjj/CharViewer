package unof.cv.tools.paramsmenu

import scala.scalajs.js
import scala.scalajs.js.Any.fromFunction1
import scala.scalajs.js.Any.fromInt

import org.scalajs.jquery.JQuery
import org.scalajs.jquery.JQueryEventObject
import org.scalajs.jquery.jQuery

import unof.cv.utils.AllKnownColors
import unof.cv.base.charLib.CMShape
import unof.cv.base.charLib.DynamicColor
import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting
import SharedPannelFunctions._
object PannelShapeColor extends ShapeExclusivePannel {

  def displayShapeParams(shape: CMShape, callbacks: CallbackCenter, settings: CvSetting) = {
    val options = callbacks.charMaker
    def setForOneColor(
      show: Boolean,
      subPannel: JQuery,
      bindingField: JQuery,
      pickingField: JQuery,
      alphaRange: JQuery,
      colorIndex: Int) {
      if (show) {
        subPannel.show(500)
        bindingField.value("None");
        pickingField.value("FFFFFF");
        val col = shape.colors(colorIndex)
        val goodV = AllKnownColors.colorThis(col.constantColor).drop(1)
        pickingField.value(goodV);
        syncColor(col.boundColor, bindingField, options);

        alphaRange.value(col.alpha.toString())
      } else {
        subPannel.hide(500)

      }
    }
    val lineSubPannel = jQuery(settings.lineColorDiv)
    val surfaceSubPannel = jQuery(settings.surfaceColorDiv)
    setForOneColor(
      shape.showSurcface,
      jQuery(settings.surfaceColorDiv),
      jQuery(settings.surfaceBoundColorInput),
      jQuery(settings.surfaceColorInput),
      jQuery(settings.surfaceAlpha),
      1)
    setForOneColor(
      shape.lineWidth > 0,
      jQuery(settings.lineColorDiv),
      jQuery(settings.lineBoundColorInput),
      jQuery(settings.lineColorInput),
      jQuery(settings.lineAlpha),
      0)
  }
  def myPannel(settings: CvSetting) = settings.shapeColorPanel
  def bind(callbacks: CallbackCenter, settings: CvSetting) {
    val lineBoundColorInput = jQuery(settings.lineBoundColorInput)
    val lineColorInput = jQuery(settings.lineColorInput)
    val lineAlpha = jQuery(settings.lineAlpha)
    val surfaceAlpha = jQuery(settings.surfaceAlpha)
    val surfaceBoundColorInput = jQuery(settings.surfaceBoundColorInput)
    val surfaceColorInput = jQuery(settings.surfaceColorInput)

    val applyShapeColorButton = jQuery(settings.shapeColorsButton)
    applyShapeColorButton.click(shapeColorSytemChanged(
      callbacks,
      lineBoundColorInput,
      lineColorInput,
      lineAlpha,
      surfaceBoundColorInput,
      surfaceColorInput,
      surfaceAlpha)_)

    surfaceBoundColorInput.on("keyup", inputInBindingField(
      1,
      callbacks,
      surfaceBoundColorInput)_)
    lineBoundColorInput.on("keyup", inputInBindingField(
      0,
      callbacks,
      lineBoundColorInput)_)
    lineColorInput.change(shapeConstColorCharge(callbacks, lineColorInput, 0)_)
    surfaceColorInput.change(shapeConstColorCharge(callbacks, surfaceColorInput, 1)_)

    lineAlpha.on("input", shapeAlphaColorCharge(callbacks, lineAlpha, 0)_)
    surfaceAlpha.on("input", shapeAlphaColorCharge(callbacks, surfaceAlpha, 1)_)
  }

  private def newColor(bondInput: JQuery, constInput: JQuery, alphaInput: JQuery) = {
    val c = AllKnownColors.colorThis(constInput.value().toString())
    val b = bondInput.value().toString()
    val a = alphaInput.value().toString().toFloat
    new DynamicColor(b, c, a)

  }
  private def inputInBindingField(
    colorIndex: Int,
    callbacks: CallbackCenter,
    bindingField: JQuery)(evt: JQueryEventObject) = {
    val boundColor = bindingField.value().toString()
    if (callbacks.currentOptions.colors.contains(boundColor)) {
      callbacks.onImageBoundColorChange(boundColor, colorIndex)
    }
  }
  private def shapeColorSytemChanged(
    callbacks: CallbackCenter,
    lineBindingField: JQuery,
    lineCstColorField: JQuery,
    lineAlpha: JQuery,
    surfBindingField: JQuery,
    surfCstColorField: JQuery,
    surfAlpha: JQuery)(evt: JQueryEventObject) = {

    val lineDynamic = newColor(lineBindingField, lineCstColorField, lineAlpha)
    val surfDynamic = newColor(surfBindingField, surfCstColorField, surfAlpha)
    if (lineDynamic.boundColor != "" && lineDynamic.boundColor != "???") {

      callbacks.onShapeColorChanged(lineDynamic, 0)
    } else
      lineBindingField.value("???")

    if (surfDynamic.boundColor != "" && surfDynamic.boundColor != "???") {
      callbacks.onShapeColorChanged(surfDynamic, 1)
    } else
      surfBindingField.value("???")
  }

  /*private def newShapeColorSytem(bindingField: JQuery, cstColorField: JQuery) = {
    def wtf(j: JQuery) = {
      j.value("???")
      ("???", "???")
    }
    val isNowBond = switch.prop("checked").toString().toBoolean
    if (isNowBond) {
      val boundColor = bindingField.value().toString()
      if (boundColor.isEmpty() || boundColor == "???")
        wtf(bindingField)
      else if (boundColor == "None") {
        switch.click();
        val cstColor = cstColorField.value().toString()
        if (cstColor.isEmpty() || cstColor == "???") {
          cstColorField.value("ffffff")
          ("C_", "white")
        } else
          ("C_", cstColor)
      } else
        ("V_", boundColor)
    } else {
      val cstColor = cstColorField.value().toString()
      if (cstColor.isEmpty() || cstColor == "???") {
        cstColorField.value("ffffff")
        ("C", "white")
      } else
        ("C_", cstColor)
    }
  }*/
  private def shapeAlphaColorCharge(
    callbacks: CallbackCenter,
    alphaField: JQuery,
    colorIndex: Int)(evt: JQueryEventObject) = {

    val alpha = alphaField.value().toString().toFloat
    callbacks.onShapeAlphaColorChange(alpha, colorIndex)

  }
  private def shapeConstColorCharge(
    callbacks: CallbackCenter,
    colorField: JQuery,
    colorIndex: Int)(evt: JQueryEventObject) = {

    val newColor = "#" + colorField.`val`().toString()
    callbacks.onShapeCstColorChange(newColor, colorIndex)

  }
}