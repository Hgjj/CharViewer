package unof.cv.tools.paramsmenu

import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting
import unof.cv.base.charmaker.CMShape
import unof.cv.base.charmaker.CMImage
import unof.cv.base.charmaker.CMPart
import unof.cv.base.charmaker.CMCategory
import unof.cv.base.charmaker.CMImage
import unof.cv.base.charmaker.CMShape

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