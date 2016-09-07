package unof.cv.tools.paramsmenu
import org.scalajs.jquery.jQuery
import org.scalajs.jquery.JQuery
import unof.cv.utils.Transforme
import scala.scalajs.js
import scala.scalajs.js.Any.fromFunction1
import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting
import SharedPannelFunctions._
object PannelRotation {
  def refresh(callbacks: CallbackCenter, settings: CvSetting) {
    val r = getTrParamInCPI(callbacks, (_:Transforme).rotation).toString()
    jQuery(settings.rotationField).value(r)

  }

  def bind(callbacks: CallbackCenter, settings: CvSetting) {
    val rotationField = jQuery(settings.rotationField)
    rotationField.on("input", onTransformChange(callbacks, rotationChange(rotationField) _)_)

  }
  private def rotationChange(jq: JQuery)(t: Transforme, z: Float) = {
    (Transforme(t.sx, t.sy, jq.value().toString().toFloat, t.dx, t.dy), z)
  }
}