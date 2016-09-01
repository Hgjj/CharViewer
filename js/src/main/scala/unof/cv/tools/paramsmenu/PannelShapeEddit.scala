package unof.cv.tools.paramsmenu

import org.scalajs.jquery.JQuery
import org.scalajs.jquery.jQuery
import unof.cv.base.charmaker.CMShape
import org.scalajs.jquery.JQueryEventObject
import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting
import unof.cv.base.charmaker.CMAdress
import unof.cv.base.charmaker.SelectShapes

object PannelShapeEddit extends ShapeExclusivePannel{
 
  def myPannel(settings: CvSetting): String =settings.shapeEditPannel
  def displayShapeParams(shape: CMShape,callbacks: CallbackCenter,settings: CvSetting): Unit = {
    val CMAdress(category,part,image,_)=callbacks.selection
    val button = jQuery(settings.shapeEditButton)
         button.empty()
         callbacks.selectedShape match {
           case Some((c, p, l)) =>
             if(c == category && p == part && l == image)
               button.append(settings.shapeEditButtonOn)
             else
               button.append(settings.shapeEditButtonOff)
           case _ => button.append(settings.shapeEditButtonOff)
         }
  }
  def bind(callbacks: CallbackCenter, settings: CvSetting) {
   val button = jQuery(settings.shapeEditButton)
   button.click(onButtonClicked(callbacks,button,settings.shapeEditButtonOn,settings.shapeEditButtonOff)_)
  }
  private def onButtonClicked(callbacks : CallbackCenter, button : JQuery,onString : String,offString : String)(evt : JQueryEventObject) = {
    val content = button.contents().prop("src").toString()

    
    button.empty()
    val isShapeSelected = callbacks.selectedShape match {
      case None => false
      case Some((c,p,s)) =>
        val selection = callbacks.selection
        c == selection.category &&
        p == selection.part &&
        s == selection.layer &&
        selection.layerSelect == SelectShapes
    }
    if(isShapeSelected){
      callbacks.setShapeSelected(false)
      button.append(offString)
    }else{
      callbacks.setShapeSelected(true)
      button.append(onString)
    }
  }
}