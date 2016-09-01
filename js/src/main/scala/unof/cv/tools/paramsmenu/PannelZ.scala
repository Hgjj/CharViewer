package unof.cv.tools.paramsmenu

import scala.scalajs.js
import scala.scalajs.js.Any.fromFunction1
import org.scalajs.jquery.JQuery
import org.scalajs.jquery.jQuery
import unof.cv.base.Transforme
import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting

object PannelZ extends SharedPannelFunctions {
  
  def refresh(callbacks: CallbackCenter, settings: CvSetting) {
    val z = 
      callbacks.selection.mapSelected(
          callbacks.charMaker,
          _.z,
          _.partZ,
          (cat)=>comonPValue(callbacks, 0f, _.partZ)
      )
     
    jQuery(settings.zField).value(""+z)
  }

  def bind(callbacks: CallbackCenter, settings: CvSetting) {
    val zField = jQuery(settings.zField)
    zField.on("input", onTransformChange(callbacks, zChange(zField) _)_)
  }
  private def zChange(jq: JQuery)(t: Transforme, z: Float): (Transforme, Float) = {
    (Transforme(t.sx, t.sy, t.rotation, t.dx, t.dy), jq.toFloat)
  }
}