package unof.cv.tools.paramsmenu

import org.scalajs.jquery.JQuery
import org.scalajs.jquery.JQueryEventObject
import org.scalajs.jquery.jQuery
import unof.cv.base.charLib.CMImage
import unof.cv.base.charLib.CharacterLibrary
import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting
import SharedPannelFunctions._
object PannelSource extends  ImageExclusivePannel {
  def myPannel(settings: CvSetting): String = settings.imageSourceDiv
  def displayImageParams(image: CMImage, callbacks: CallbackCenter, settings: CvSetting): Unit = {

    updateSuggestionList(callbacks.charMaker, settings)
    val refField = jQuery(settings.sourceSelectionField)
    refField.value("")

    val refSpan = jQuery(settings.currentRefSpanId)
    refSpan.empty()
    val layerName = image.toString()
    refSpan.append("<i>" + layerName + "</i>")
  }
  def bind(callbacks: CallbackCenter, settings: CvSetting) {
    val refField = jQuery(settings.sourceSelectionField)
    jQuery(settings.changeImageButton).click(sourceChange(callbacks, refField) _)
  }
  private def updateSuggestionList(options: CharacterLibrary, settings: CvSetting) {
    val newList = options.imageMap.toSeq
      .map(_._1)
      .flatMap {
        s =>
          val split = s.split("/")
          (1 to split.size).map(split.take(_).mkString("/"))
      }.toSet.toSeq
    val listElem = jQuery(settings.fileSuggestionList)
    setOptionsInList(newList :+ "None", listElem)
  }
  private def sourceChange(callback: CallbackCenter, sourceIn: JQuery)(evt: JQueryEventObject) = {

    val newSource = sourceIn.value().toString.trim
    if (newSource.isEmpty() || newSource == "???")
      sourceIn.`val`("???")
    else {
      callback.onImageRefChanged(getSource(newSource))
    }

  }

}