package unof.cv.tools.paramsmenu

import java.util.regex.Pattern

import scala.scalajs.js
import scala.scalajs.js.Any.fromFunction1
import scala.scalajs.js.Any.fromString

import org.scalajs.jquery.JQuery
import org.scalajs.jquery.JQueryEventObject
import org.scalajs.jquery.jQuery

import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting
import SharedPannelFunctions._
object PannelImport extends LayerTypeInsensitvePannel with BasicPannel {

  def myPannel(settings: CvSetting): String = settings.imagesImportDiv

  def ifCategorySelected(callbacks: CallbackCenter, settings: CvSetting, cat: unof.cv.base.charLib.CMCategory): Unit = {
    show(settings)
    val importLayerButton = jQuery(settings.importLayersButton)
    importLayerButton.empty()
    importLayerButton.append("Import as parts")
  }
  def ifLayerSelected(callbacks: CallbackCenter, settings: CvSetting, image: unof.cv.base.charLib.CMLayer): Unit = {
    hide(settings)
  }
  def ifPartSelected(callbacks: CallbackCenter, settings: CvSetting, part: unof.cv.base.charLib.CMPart): Unit = {
    show(settings)
    val importLayerButton = jQuery(settings.importLayersButton)
    importLayerButton.empty()
    importLayerButton.append("Import as images")
  }

  def bind(callbacks: CallbackCenter, settings: CvSetting) {
    val layersInput = jQuery(settings.partMenuImportInput)
    jQuery(settings.importLayersButton).click(addManyFiles(callbacks, layersInput)_)

  }
  private def addManyFiles(callbacks: CallbackCenter, input: JQuery)(evt: JQueryEventObject) = {
    val inString = input.value().toString().trim()
    if (inString.contains("\"")) {
      val p = Pattern.compile("\"[^\"]+\"")
      val matches = p.matcher(inString)
      if (matches.find()) {
        val seq = Iterator
          .continually(matches.group())
          .takeWhile { s => matches.find() }
          .map(getSource(_))
          .toSeq
        if (!seq.isEmpty)
          callbacks.onManyLayersImported(seq)
      }
    } else {
      callbacks.onManyLayersImported(Seq(getSource(inString.trim())))
    }

  }

}