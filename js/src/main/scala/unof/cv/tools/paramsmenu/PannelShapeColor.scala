package unof.cv.tools.paramsmenu

import scala.scalajs.js
import scala.scalajs.js.Any.fromBoolean
import scala.scalajs.js.Any.fromFunction1
import scala.scalajs.js.Any.fromInt

import org.scalajs.jquery.JQuery
import org.scalajs.jquery.JQueryEventObject
import org.scalajs.jquery.jQuery

import unof.cv.base.AllKnownColors
import unof.cv.base.charmaker.BoundColor
import unof.cv.base.charmaker.CMShape
import unof.cv.base.charmaker.ConstantColor
import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting

object PannelShapeColor extends SharedPannelFunctions with ShapeExclusivePannel {
  
  def displayShapeParams(shape: CMShape, callbacks: CallbackCenter, settings: CvSetting) = {
    val options = callbacks.charMaker
    def setForOneColor(
      show: Boolean,
      subPannel: JQuery,
      bindingField: JQuery,
      pickingField: JQuery,
      alphaRange: JQuery,
      switch: JQuery,
      colorIndex: Int) {
      if (show) {
        subPannel.show(500)
        bindingField.value("None");
        pickingField.value("FFFFFF");
        val col = shape.colors(colorIndex)
        col match {
          case ConstantColor(v) =>
            bindingField.hide(500);
            pickingField.show(500)
            val goodV = AllKnownColors.colorThis(v).drop(1)
            pickingField.value(goodV);
            switch.prop("checked", false)
          case BoundColor(b) =>
            bindingField.show(500);
            pickingField.hide(500)
            switch.prop("checked", true)
            syncColor(b, bindingField, options);

        }
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
      jQuery(settings.surfaceIsBoundCheckbox),
      1)
    setForOneColor(
      shape.lineWidth > 0,
      jQuery(settings.lineColorDiv),
      jQuery(settings.lineBoundColorInput),
      jQuery(settings.lineColorInput),
      jQuery(settings.lineAlpha),
      jQuery(settings.lineIsBoundCheckbox),
      0)
  }
  def myPannel(settings: CvSetting) = settings.shapeColorPanel
  def bind(callbacks: CallbackCenter, settings: CvSetting) {
    val lineBoundColorInput = jQuery(settings.lineBoundColorInput)
    val lineColorInput = jQuery(settings.lineColorInput)
    val lineIsBoundCheckbox = jQuery(settings.lineIsBoundCheckbox)
    val surfaceBoundColorInput = jQuery(settings.surfaceBoundColorInput)
    val surfaceColorInput = jQuery(settings.surfaceColorInput)
    val surfaceIsBoundCheckbox = jQuery(settings.surfaceIsBoundCheckbox)

    lineIsBoundCheckbox.click(colorDynamicTypeSwitched(callbacks, lineIsBoundCheckbox, lineBoundColorInput, lineColorInput, 0)_)
    surfaceIsBoundCheckbox.click(colorDynamicTypeSwitched(callbacks, surfaceIsBoundCheckbox, surfaceBoundColorInput, surfaceColorInput, 1)_)

    val applyShapeColorButton = jQuery(settings.shapeColorsButton)
    applyShapeColorButton.click(shapeColorSytemChanged(
      callbacks,
      lineBoundColorInput,
      lineColorInput,
      lineIsBoundCheckbox,
      surfaceBoundColorInput,
      surfaceColorInput,
      surfaceIsBoundCheckbox)_)
    surfaceBoundColorInput.on("keyup", inputInBindingField(
      1,
      callbacks,
      surfaceBoundColorInput,
      surfaceIsBoundCheckbox)_)
    lineBoundColorInput.on("keyup", inputInBindingField(
      0,
      callbacks,
      lineBoundColorInput,
      lineIsBoundCheckbox)_)
    lineColorInput.change(shapeConstColorCharge(callbacks, lineIsBoundCheckbox, lineColorInput, 0)_)
    surfaceColorInput.change(shapeConstColorCharge(callbacks, surfaceIsBoundCheckbox, surfaceColorInput, 1)_)

    val lineAlpha = jQuery(settings.lineAlpha)
    lineAlpha.on("input",shapeAlphaColorCharge(callbacks, lineAlpha, 0)_)
    val surfaceAlpha = jQuery(settings.surfaceAlpha)
    surfaceAlpha.on("input",shapeAlphaColorCharge(callbacks, surfaceAlpha, 1)_)
  }

  private def colorDynamicTypeSwitched(callbacks: CallbackCenter, switch: JQuery, bondInput: JQuery, constInput: JQuery, colorIndex: Int)(evt: JQueryEventObject) {
    val isNowBound = switch.prop("checked").toString.toBoolean
    println("Panshape switch bound " +isNowBound)
    if (isNowBound) {
      bondInput.show(500)
      constInput.hide(500)
      syncColor(bondInput.value().toString(), bondInput, callbacks.charMaker)
      val boundColor = bondInput.value().toString()
      if (callbacks.currentOptions.colors.contains(boundColor)) {
        callbacks.onImageBoundColorChange("V_" + boundColor, colorIndex)
      }
    } else {
      bondInput.hide(500)
      constInput.show(500)
      syncColor("None", bondInput, callbacks.charMaker)
      val in = "C_" + AllKnownColors.colorThis(constInput.value().toString());
      callbacks.onImageBoundColorChange(in, colorIndex)
    }
  }
  private def inputInBindingField(
    colorIndex: Int,
    callbacks: CallbackCenter,
    bindingField: JQuery,
    switch: JQuery)(evt: JQueryEventObject) = {
    val isNowBound = switch.prop("checked").toString.toBoolean
    if (isNowBound) {
      val boundColor = bindingField.value().toString()
      if(boundColor == "None")
        callbacks.onImageBoundColorChange("C_white", colorIndex)
      else if (callbacks.currentOptions.colors.contains(boundColor)) {
        callbacks.onImageBoundColorChange("V_" + boundColor, colorIndex)
      }
    }

  }
  private def shapeColorSytemChanged(
    callbacks: CallbackCenter,
    lineBindingField: JQuery,
    lineCstColorField: JQuery,
    lineSwitch: JQuery,
    surfBindingField: JQuery,
    surfCstColorField: JQuery,
    surfSwitch: JQuery)(evt: JQueryEventObject) = {

    val (lineCode, lineValue) = newShapeColorSytem(lineBindingField, lineCstColorField, lineSwitch)
    val (surfCode, surfValue) = newShapeColorSytem(surfBindingField, surfCstColorField, surfSwitch)
    if (lineCode != "???") {

      callbacks.onImageBoundColorChange(lineCode + lineValue, 0)
    }

    if (surfCode != "???") {
      callbacks.onImageBoundColorChange(surfCode + surfValue, 1)
    }
  }

  private def newShapeColorSytem(bindingField: JQuery, cstColorField: JQuery, switch: JQuery) = {
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
  }
  private def shapeAlphaColorCharge(
    callbacks: CallbackCenter,
    alphaField: JQuery,
    colorIndex: Int)(evt: JQueryEventObject) = {

    val alpha = alphaField.value().toString().toFloat
    callbacks.onShapeAlphaColorChange(alpha, colorIndex)

  }
  private def shapeConstColorCharge(
    callbacks: CallbackCenter,
    switch: JQuery,
    colorField: JQuery,
    colorIndex: Int)(evt: JQueryEventObject) = {
    if (!switch.is(":checked")) {

      val newColor = "#" + colorField.`val`().toString()
      callbacks.onShapeCstColorChange(newColor, colorIndex)
    }

  }
}