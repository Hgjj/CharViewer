package unof.cv.tools.paramsmenu

import scala.scalajs.js
import scala.scalajs.js.Any.fromBoolean
import scala.scalajs.js.Any.fromFunction1

import org.scalajs.jquery.JQuery
import org.scalajs.jquery.JQueryEventObject
import org.scalajs.jquery.jQuery

import unof.cv.base.charmaker.CMShape
import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting

object PannelShapeProperties extends SharedPannelFunctions with ShapeExclusivePannel {

  def myPannel(settings: CvSetting): String = settings.shapeParamsPannel
  
  def displayShapeParams(shape: CMShape, callbacks: CallbackCenter, settings: CvSetting): Unit = {
    val joinSelect = jQuery(settings.lineJoinSelect)
    joinSelect.value(shape.lineJoint)
    val surfaceSwitch = jQuery(settings.fillShapeSwitch)
    surfaceSwitch.prop("checked", shape.showSurcface)
    val lineWidthIn = jQuery(settings.lineWidth)
    lineWidthIn.value(shape.lineWidth.toString())
    val closeSwitch = jQuery(settings.closeShapeShwitch)
    closeSwitch.prop("checked", shape.closed)
  }

  def bind(callbacks: CallbackCenter, settings: CvSetting) {
    val surfaceSwitch = jQuery(settings.fillShapeSwitch)
    surfaceSwitch.click(fillSwitchClicked(callbacks, surfaceSwitch)_)
    val joinSelect = jQuery(settings.lineJoinSelect)
    joinSelect.change(lineJoinChanged(callbacks, joinSelect)_)
    val lineWidthIn = jQuery(settings.lineWidth)
    lineWidthIn.change(lineWidthChanged(callbacks, lineWidthIn)_)
    val closeSwitch = jQuery(settings.closeShapeShwitch)
    closeSwitch.click(closeSwitchClicked(callbacks, closeSwitch)_)
  }
  private def lineWidthChanged(callbacks: CallbackCenter, lineWidthIn: JQuery)(evt: JQueryEventObject) {
    val newWidth = lineWidthIn.value().toString().toInt
    callbacks.onLayerChanged { onlyForShape { _.setLineWidth(newWidth) } }
  }

  private def lineJoinChanged(callbacks: CallbackCenter, joinSelect: JQuery)(evt: JQueryEventObject) {
    val newJoin = joinSelect.value().toString()
    callbacks.onLayerChanged { onlyForShape { _.setLineJoint(newJoin) } }
  }
  private def fillSwitchClicked(callbacks: CallbackCenter, switch: JQuery)(evt: JQueryEventObject) {
    val surfIsNowShown = switch.prop("checked").toString().toBoolean
    def f(s: CMShape) = {
      s.setShowSurface(surfIsNowShown)
    }
    callbacks.onLayerChanged { onlyForShape { f } }
  }
  private def closeSwitchClicked(callbacks: CallbackCenter, switch: JQuery)(evt: JQueryEventObject) {
    val shapeNowClosed = switch.prop("checked").toString().toBoolean
    def f(s: CMShape) = {
      s.setClosed(shapeNowClosed)
    }
    callbacks.onLayerChanged { onlyForShape { f } }
  }
}