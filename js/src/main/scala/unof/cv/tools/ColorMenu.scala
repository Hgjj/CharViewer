package unof.cv.tools

import org.scalajs.jquery.jQuery
import org.scalajs.jquery.JQueryEventObject
import unof.cv.utils.AllKnownColors
import scala.scalajs.js.Dynamic

object ColorMenu {
   def createMenuColor(settings : CvSetting, callback : CallbackCenter) = {
    val colorDivName = settings.colorsMenuComponent
    val colorCount =settings.colorSlotsCount.intValue()
    val colorDiv = jQuery(colorDivName)
    (0 to colorCount-1) foreach {
      i =>
        val colorAlias = "cDiv"+i
        val cnDiv = jQuery("<div class=\"popupElem\">")
        val colorPersoDiv = jQuery("<div id=\"span"+colorAlias+"\">")
        val colorNameSpan = jQuery("<span id=\"panpan"+colorAlias+"\">")
        val colorElem = jQuery("<input type=\"text\" class=\"jscolor colorVariable\" name=\"" + colorAlias + "\" id=\"" + colorAlias + "\" size=\"8\">")
        
        colorElem.change(callback.onColorChange(i, _: JQueryEventObject))
        colorPersoDiv.append("<hr>")
        colorPersoDiv.append(cnDiv)
        cnDiv.append(colorElem)
        cnDiv.append(colorNameSpan)
        colorDiv.append(colorPersoDiv)
    }  
      
  }
  def drawColorMenu(setting : CvSetting,  newColorNames: Seq[String], newColors : Seq[String]) = {
    val colorDivName = setting.colorsMenuComponent
    val colorDiv = jQuery(colorDivName)
    
   
   
      if(newColorNames.isEmpty || newColors.isEmpty){
        (0 to setting.colorSlotsCount.intValue() -1) foreach {
          i=>jQuery("#spancDiv"+i).hide(500)
        }
      }
      else {
        (newColorNames.zip(newColors)).zipWithIndex foreach {
          case((name,value),i) =>
            val colorAlias = "cDiv"+i
            val colorSpan = jQuery("#span"+colorAlias)
            colorSpan.show(500)
            val colorNameSpan = jQuery("#panpan"+colorAlias)
            colorNameSpan.empty()
            colorNameSpan.append(name + " : ")
        }
        (newColorNames.size to setting.colorSlotsCount.intValue() -1) foreach {
          i=>jQuery("#spancDiv"+i).hide(500)
        }
      }
  }
  
  def updateColorColors(colorMask: Seq[String]) =  {
    colorMask.zipWithIndex foreach {
      case(value,i) =>
        val colorAlias = "cDiv"+i
        val codeVal = AllKnownColors.get(value)match{
          case None => value.filter { _ !='#' }
          case Some(code) => code.drop(1)
        }
        
        Dynamic.global.document.getElementById(colorAlias)
          .jscolor.fromString(codeVal)
    }
  }
 
}