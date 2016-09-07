package unof.cv.tools.paramsmenu

import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting
import unof.cv.base.charLib.CMShape
import unof.cv.base.charLib.CMImage
import unof.cv.base.charLib.CMPart
import unof.cv.base.charLib.CMCategory
import unof.cv.base.charLib.CMImage
import unof.cv.base.charLib.CMShape

trait ImagePannel {
   def refresh(callbacks: CallbackCenter, settings: CvSetting) = {
    callbacks.selection.forSelected(callbacks.charMaker,
        {i:CMImage => show(settings);setImageParams(callbacks, settings, i)},
        (s:CMShape) => hide(settings),
        (p : CMPart)=> hide(settings),
        (c : CMCategory)=>hide(settings)
     )
  }

  def setImageParams(callbacks: CallbackCenter, settings: CvSetting, shape: CMImage)

  def hide(settings: CvSetting)
  def show(settings: CvSetting)
}