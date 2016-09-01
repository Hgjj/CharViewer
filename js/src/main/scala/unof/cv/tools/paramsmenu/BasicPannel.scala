package unof.cv.tools.paramsmenu

import unof.cv.tools.CvSetting
import org.scalajs.jquery.jQuery

trait BasicPannel {
  def myPannel(settings: CvSetting) : String
  def show(settings: CvSetting) = jQuery(myPannel(settings)).show(500)
  def hide(settings: CvSetting) = jQuery(myPannel(settings)).hide(500)
}