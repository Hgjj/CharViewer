package unof.cv.tools.paramsmenu

import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting
import unof.cv.base.charLib.CMImage
import unof.cv.base.charLib.CMShape
import unof.cv.base.charLib.CMPart
import unof.cv.base.charLib.CMCategory

trait LayerTypeSensitivePannel {
  final def refresh(callbacks: CallbackCenter, settings: CvSetting) = {
    callbacks.selection.forSelected(callbacks.charMaker,
        ifImageSelected(callbacks, settings, _),
        ifShapeSelected(callbacks, settings, _),
        ifPartSelected(callbacks, settings, _),
        ifCategorySelected(callbacks, settings, _)
     )
  }

  def ifImageSelected(callbacks: CallbackCenter, settings: CvSetting, image: CMImage)
  def ifShapeSelected(callbacks: CallbackCenter, settings: CvSetting, shape: CMShape)
  def ifPartSelected(callbacks: CallbackCenter, settings: CvSetting, part: CMPart)
  def ifCategorySelected(callbacks: CallbackCenter, settings: CvSetting, cat: CMCategory)
}