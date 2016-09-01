package unof.cv.tools.paramsmenu

import scala.scalajs.js
import scala.scalajs.js.Any.fromFunction1
import scala.scalajs.js.Any.fromInt
import org.scalajs.jquery.JQuery
import org.scalajs.jquery.JQueryEventObject
import org.scalajs.jquery.jQuery
import unof.cv.base.charmaker.CMImage
import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting
import unof.cv.tools.CvSetting

object PannelColor extends SharedPannelFunctions with ImageExclusivePannel {

  def displayImageParams(image: CMImage, callbacks: CallbackCenter, settings: CvSetting): Unit = {
    val imageColor =
      image.boundColor
    val colorBinder = jQuery(settings.colorBindingField)
    syncColor(imageColor, colorBinder, callbacks.currentOptions);
  }
  def myPannel(settings: CvSetting) = settings.colorPanel
  def bind(callbacks: CallbackCenter, settings: CvSetting) {
    jQuery(settings.colorBindingButton).click(colorChange(callbacks, jQuery(settings.colorBindingField))_)
  }
  private def colorChange(callback: CallbackCenter, colorIn: JQuery)(evt: JQueryEventObject) = {
    val newColor = colorIn.value().toString()
    callback.onImageBoundColorChange(newColor, 0)
  }
}