package unof.cv.tools.paramsmenu

import scala.scalajs.js
import scala.scalajs.js.Any.fromFunction1
import scala.scalajs.js.Any.fromInt
import org.scalajs.jquery.JQuery
import org.scalajs.jquery.JQueryEventObject
import org.scalajs.jquery.jQuery
import unof.cv.base.charLib.CMImage
import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting
import unof.cv.tools.CvSetting

object PannelColor extends ImageExclusivePannel {

  def displayImageParams(image: CMImage, callbacks: CallbackCenter, settings: CvSetting): Unit = {
    val imageColor =
      image.boundColor
    val colorBinder = jQuery(settings.colorBindingField)
    SharedPannelFunctions.syncColor(imageColor, colorBinder, callbacks.currentOptions);
    
    
    val colorAlpha = jQuery(settings.imageAlpha)
    colorAlpha.value(""+image.alpha)
  }
  def myPannel(settings: CvSetting) = settings.colorPanel
  def bind(callbacks: CallbackCenter, settings: CvSetting) {
    jQuery(settings.colorBindingButton).click(colorChange(callbacks, jQuery(settings.colorBindingField))_)
    val colorAlpha = jQuery(settings.imageAlpha)
    colorAlpha.on("input",oneAlphaInput(callbacks, colorAlpha)_)
  }
  private def colorChange(callback: CallbackCenter, colorIn: JQuery)(evt: JQueryEventObject) = {
    val newColor = colorIn.value().toString()
    callback.onImageBoundColorChange(newColor, 0)
  }
  private def oneAlphaInput(callback: CallbackCenter, alphaIn: JQuery)(evt: JQueryEventObject) = {
    val newAlpha = alphaIn.value().toString().toFloat
    callback.onAlphaColorChange(newAlpha, 0)
  }
}