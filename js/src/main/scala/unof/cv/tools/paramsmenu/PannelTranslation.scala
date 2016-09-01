package unof.cv.tools.paramsmenu

import scala.scalajs.js
import scala.scalajs.js.Any.fromFunction1

import org.scalajs.jquery.JQuery
import org.scalajs.jquery.jQuery

import unof.cv.base.Transforme
import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting

object PannelTranslation extends SharedPannelFunctions {

  def refresh(callbacks: CallbackCenter, settings: CvSetting) {
    val dx = getTrParamInCPI(callbacks, _.dx)
    val dy = getTrParamInCPI(callbacks, _.dy)

    jQuery(settings.xTranslationField).value(""+dx)
    jQuery(settings.yTranslationField).value(""+dy)
  }

  def bind(callbacks: CallbackCenter, settings: CvSetting) {
    val dxField = jQuery(settings.xTranslationField)
    val dyField = jQuery(settings.yTranslationField)
    dxField.on("input", onTransformChange(callbacks, dxChange(dxField) _)_)
    dyField.on("input", onTransformChange(callbacks, dyChange(dyField) _)_)
  }
  private def dxChange(jq: JQuery)(t: Transforme, z: Float) = {
    (Transforme(t.sx, t.sy, t.rotation, jq, t.dy), z)
  }
  private def dyChange(jq: JQuery)(t: Transforme, z: Float) = {
    (Transforme(t.sx, t.sy, t.rotation, t.dx, jq), z)
  }
}