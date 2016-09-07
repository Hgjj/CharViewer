package unof.cv.tools.paramsmenu

import scala.scalajs.js
import org.scalajs.jquery.jQuery
import unof.cv.base.charLib.AlwayVisible
import unof.cv.base.charLib.LinkedVisibility
import unof.cv.base.charLib.VisibilityCondition
import unof.cv.base.charLib.VisibleIfNoLink
import unof.cv.tools.CvSetting
import unof.cv.tools.CallbackCenter
import org.scalajs.jquery.JQueryEventObject
import SharedPannelFunctions._
object ParMenuDrawer {

  def update(settings: CvSetting, callbacks: CallbackCenter): Unit = {
   
    val imgMenu = jQuery(settings.imgMenuComponent)
    val options = callbacks.currentOptions
    val category = callbacks.currentSelection.category
    if (category < 0 || ! settings.devMod) {
      imgMenu.hide()
    } else {

      imgMenu.show()
      
      val selectedName = callbacks.selection.nameSelected(callbacks.charMaker)
      val nameDiv = jQuery(settings.elementName)
      nameDiv.empty()
      nameDiv.append(selectedName)

      PannelColor.refresh(callbacks, settings)
      PannelComponents.refresh(callbacks, settings)
      PannelCondition.refresh(callbacks, settings)
      PannelDeltas.refresh(callbacks, settings)
      PannelImport.refresh(callbacks, settings)
      PannelLocation.refresh(callbacks, settings)
      PannelRename.refresh(callbacks, settings)
      PannelRotation.refresh(callbacks, settings)
      PannelScale.refresh(callbacks, settings)
      PannelShapeColor.refresh(callbacks, settings)
      PannelShapeEddit.refresh(callbacks, settings)
      PannelShapeProperties.refresh(callbacks, settings)
      PannelSource.refresh(callbacks, settings)
      PannelTranslation.refresh(callbacks, settings)
      PannelZ.refresh(callbacks, settings)

      updateColorSuggestion

      def updateColorSuggestion = {
        val colorList = jQuery(settings.colorsSuggestionList)
        setOptionsInList(options.colors :+ "None", colorList)
      }
    }
  }

  def bindComponents(settings: CvSetting, callbacks: CallbackCenter) = {
    val options = callbacks.currentOptions
    PannelColor.bind(callbacks, settings)
    PannelComponents.bind(callbacks, settings)
    PannelCondition.bind(callbacks, settings)
    PannelDeltas.bind(callbacks, settings)
    PannelImport.bind(callbacks, settings)
    PannelLocation.bind(callbacks, settings)
    PannelRename.bind(callbacks, settings)
    PannelRotation.bind(callbacks, settings)
    PannelScale.bind(callbacks, settings)
    PannelShapeColor.bind(callbacks, settings)
    PannelShapeEddit.bind(callbacks, settings)
    PannelShapeProperties.bind(callbacks, settings)
    PannelSource.bind(callbacks, settings)
    PannelTranslation.bind(callbacks, settings)
    PannelZ.bind(callbacks, settings)

    val delButton = jQuery(settings.deletImageButton)
    delButton.click(deletImage(callbacks)_)

    val copyButton = jQuery(settings.copyImageButton)
    copyButton.click(copyImage(callbacks)_)
  }

  private def deletImage(callback: CallbackCenter)(evt: JQueryEventObject) = {
    callback.onImageDeleted
  }
  private def copyImage(callback: CallbackCenter)(evt: JQueryEventObject) = {

    callback.onImageCopyed()
  }

}