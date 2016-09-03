package unof.cv.tools.paramsmenu

import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting
import org.scalajs.jquery.jQuery
import org.scalajs.jquery.JQuery
import org.scalajs.jquery.JQueryEventObject
import scala.scalajs.js.Dynamic

object PannelRename {
  def refresh(callbacks: CallbackCenter, settings: CvSetting) {
     jQuery(settings.renameInput).value("")
  }
  def bind(callbacks: CallbackCenter, settings: CvSetting){
    jQuery(settings.renameButton).click(onRename(callbacks,jQuery(settings.renameInput)) _)
  }
  
  private def onRename(callbacks: CallbackCenter, input : JQuery)(evt:JQueryEventObject)={
    val newName = input.value().toString()
    val err = callbacks.askNameChange(newName)
    if(err != "")
      Dynamic.global.prompt(err);
  }
}