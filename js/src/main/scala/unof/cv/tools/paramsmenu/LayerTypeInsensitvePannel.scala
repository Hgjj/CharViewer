package unof.cv.tools.paramsmenu

import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting
import unof.cv.base.charLib.CMImage
import unof.cv.base.charLib.CMShape
import unof.cv.base.charLib.CMPart
import unof.cv.base.charLib.CMCategory
import unof.cv.base.charLib.CMLayer

trait LayerTypeInsensitvePannel {
  def refresh(callbacks: CallbackCenter, settings: CvSetting) = {
    callbacks.selection.forSelected(callbacks.charMaker,
        ifLayerSelected(callbacks, settings, _),
        ifPartSelected(callbacks, settings, _),
        ifCategorySelected(callbacks, settings, _)
     )
  }

  def ifLayerSelected(callbacks: CallbackCenter, settings: CvSetting, image: CMLayer)
  def ifPartSelected(callbacks: CallbackCenter, settings: CvSetting, part: CMPart)
  def ifCategorySelected(callbacks: CallbackCenter, settings: CvSetting, cat: CMCategory)
}