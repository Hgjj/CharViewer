package unof.cv.tools.paramsmenu

import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting
import unof.cv.base.charmaker.CMShape
import unof.cv.base.charmaker.CMImage
import unof.cv.base.charmaker.CMShape
import unof.cv.base.charmaker.CMPart
import unof.cv.base.charmaker.CMCategory

trait ShapePannel {
  def refresh(callbacks: CallbackCenter, settings: CvSetting) = {
    callbacks.selection.forSelected(callbacks.charMaker,
        (i:CMImage) => hide(settings),
        {s :CMShape => show(settings);setShapeParams(callbacks, settings, s)},
        (p : CMPart)=> hide(settings),
        (c : CMCategory)=>hide(settings)
     )
  }

  def setShapeParams(callbacks: CallbackCenter, settings: CvSetting, shape: CMShape)

  def hide(settings: CvSetting)
  def show(settings: CvSetting)
}