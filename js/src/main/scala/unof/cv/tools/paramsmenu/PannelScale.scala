package unof.cv.tools.paramsmenu
import org.scalajs.jquery.jQuery
import org.scalajs.jquery.JQuery
import unof.cv.base.Transforme
import scala.scalajs.js
import scala.scalajs.js.Any.fromFunction1
import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting

object PannelScale extends SharedPannelFunctions {
  def refresh(callbacks: CallbackCenter, settings: CvSetting) {

    val sx = getTrParamInCPI(callbacks, _.sx).toString()
    val sy = getTrParamInCPI(callbacks, _.sy).toString()

    jQuery(settings.xScaleField).value(sx)
    jQuery(settings.yScaleField).value(sy)

  }

  def bind(callbacks: CallbackCenter, settings: CvSetting) {
    val sxField = jQuery(settings.xScaleField)

    val syField = jQuery(settings.yScaleField)
    sxField.on("input", onTransformChange(callbacks, sxChange(sxField) _)_)
    syField.on("input", onTransformChange(callbacks, syChange(syField) _)_)
  }
  private def sxChange(jq: JQuery)(t: Transforme, z: Float) = {
    (Transforme(jq, t.sy, t.rotation, t.dx, t.dy), z)
  }
  private def syChange(jq: JQuery)(t: Transforme, z: Float) = {
    (Transforme(t.sx, jq, t.rotation, t.dx, t.dy), z)
  }
}