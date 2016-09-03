package unof.cv.tools.paramsmenu

import unof.cv.base.charmaker.CMPart
import org.scalajs.jquery.jQuery
import org.scalajs.jquery.JQuery
import org.scalajs.jquery.JQueryEventObject
import unof.cv.base.charmaker.CMAdress
import unof.cv.base.charmaker.LayersSelector
import unof.cv.base.charmaker.SelectImages
import scala.scalajs.js
import scala.scalajs.js.Any.fromFunction1
import scala.scalajs.js.Any.fromInt
import scala.scalajs.js.Any.fromString
import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting
import unof.cv.base.charmaker.SelectShapes
import scala.scalajs.js.Dynamic

object PannelComponents {
  def refresh(callbacks: CallbackCenter, settings: CvSetting) {
    val CMAdress(category, part, image, select) = callbacks.selection
    val options = callbacks.currentOptions

    val componentsPannel = jQuery(settings.elementComponentDiv)
    val addShapeButton = jQuery(settings.newShapeButton)
    val addImageButton = jQuery(settings.newImageButton)
    val addPartButton = jQuery(settings.newPartButton)
    if (image < 0) {
      componentsPannel.show(500)
      val partList = jQuery(settings.partMenuLayerList)
      partList.empty()

      def layersOfAPart(selectedPart: CMPart, dl: JQuery, pIndex: Int = -1) = {
        def printImg(nameindex: (String, Int), layerSelect: LayersSelector) = {
          val dd = jQuery("<dd>")
          val (name, index) = nameindex
          val imgName = "<ins>" + name + "</ins>"
          dd.append(imgName)
          if (pIndex < 0)
            dd.click(setSelectedImage(callbacks, index, layerSelect) _)
          else
            dd.click(setSelectedImage(callbacks, pIndex, index, layerSelect) _)
          dl.append(dd)
        }
        selectedPart.images.map("Image " + _.name).zipWithIndex.foreach(printImg(_, SelectImages))
        selectedPart.shapes.map("Shape " + _.name).zipWithIndex.foreach(printImg(_, SelectShapes))

      }

      if (part < 0) {
        addShapeButton.hide(500)
        addImageButton.hide(500)
        addPartButton.show(500)
        options.categories(category).possibleParts.zipWithIndex.foreach {
          case (part, index) =>
            val dl = jQuery("<dl>")
            val dt = jQuery("<dt>")
            dt.append("<b>" + part.partName + " : </b>")
            dt.click(setSelectedPart(callbacks, index)_)
            dl.append(dt)
            layersOfAPart(part, dl, index)
            partList.append(dl)
        }
      } else {
        addShapeButton.show(500)
        addImageButton.show(500)
        addPartButton.hide(500)
        val selectedPart = options.categories(category).possibleParts(part)
        val dl = jQuery("<dl>")
        val dt = jQuery("<dt>")
        dt.append("<b>Layers : </b>")
        dl.append(dt)
        layersOfAPart(selectedPart, dl)
        partList.append(dl)
      }
    } else {
      componentsPannel.hide(500)
    }
  }

  def bind(callbacks: CallbackCenter, settings: CvSetting) {
    jQuery(settings.newPartButton).click(newPart(callbacks) _)
    jQuery(settings.newShapeButton).click(newShape(callbacks)_)
    jQuery(settings.newImageButton).click(newImage(callbacks) _)
  }
  private def setSelectedImage(callback: CallbackCenter, partIndex: Int, imgIndex: Int, select: LayersSelector)(evt: JQueryEventObject) = {
    callback.onLayerSelected(partIndex, imgIndex, select)
  }
  private def setSelectedImage(callback: CallbackCenter, imgIndex: Int, select: LayersSelector)(evt: JQueryEventObject) = {
    callback.onLayerSelected(imgIndex, select)
  }
  private def setSelectedPart(callback: CallbackCenter, partIndex: Int)(evt: JQueryEventObject) = {
    callback.onPartSelected(partIndex)
  }
  private def newPart(callbacks : CallbackCenter)(evt:JQueryEventObject)= {
    val res = Dynamic.global.prompt("Name this new ¨Part","")
    if(res!=null)
      callbacks.onPartCreated(res.toString())
  }
  private def newShape(callbacks : CallbackCenter)(evt:JQueryEventObject)= {
    callbacks.onShapeCreated
  }
  private def newImage(callbacks : CallbackCenter)(evt:JQueryEventObject)= {
    callbacks.onImageCreated
  }
}