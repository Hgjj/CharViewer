package unof.cv.tools.paramsmenu

import unof.cv.tools.CvSetting
import unof.cv.base.charLib.CMImage
import unof.cv.tools.CallbackCenter
import unof.cv.base.charLib.CMCategory
import unof.cv.base.charLib.CMPart
import unof.cv.base.charLib.CMShape
import org.scalajs.jquery.jQuery

trait ImageExclusivePannel extends LayerTypeSensitivePannel with BasicPannel {
  final def ifCategorySelected(callbacks: CallbackCenter, settings: CvSetting, cat: CMCategory): Unit =
    hide(settings)
  final def ifImageSelected(callbacks: CallbackCenter, settings: CvSetting, image: CMImage): Unit = {
    show(settings)
    displayImageParams(image, callbacks, settings)
  }
  final def ifPartSelected(callbacks: CallbackCenter, settings: CvSetting, part: CMPart): Unit =
    hide(settings)
  final def ifShapeSelected(callbacks: CallbackCenter, settings: CvSetting, shape: CMShape): Unit =
    hide(settings)
  def displayImageParams(image: CMImage, callbacks: CallbackCenter, settings: CvSetting)
  
}