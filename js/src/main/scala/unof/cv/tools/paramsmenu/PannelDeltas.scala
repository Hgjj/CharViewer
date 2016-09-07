package unof.cv.tools.paramsmenu

import scala.scalajs.js
import scala.scalajs.js.Any.fromBoolean
import scala.scalajs.js.Any.fromFunction1
import scala.scalajs.js.Any.fromInt

import org.scalajs.jquery.JQuery
import org.scalajs.jquery.JQueryEventObject
import org.scalajs.jquery.jQuery

import unof.cv.utils.AllKnownColors
import unof.cv.base.charLib.CMShape
import unof.cv.tools.CallbackCenter
import unof.cv.tools.CvSetting
import unof.cv.base.charLib.SelectShapes
import unof.cv.base.charLib.DeltaLink
import SharedPannelFunctions._

object PannelDeltas extends ShapeExclusivePannel  {
  def myPannel(settings: CvSetting): String = settings.deltasDiv

  def displayShapeParams(shape: CMShape, callbacks: CallbackCenter, settings: CvSetting): Unit = {
    val charmaker = callbacks.charMaker
    val deltaList = jQuery(settings.tranformationList)
    val sliderBinding = jQuery(settings.tranformationSliderSelect)
    val position = jQuery(settings.tranformationPosition)
    val sliderSuggestion = jQuery(settings.sliderList)
    val pos = charmaker.locationMap(shape.id)
    val p = charmaker.getPart(pos.category, pos.part)
    val deltaListElemts = p.shapes.zipWithIndex.filter { t => t._1.deltaLink.key == shape.deltaLink.key }
    val tranformationSourceList = jQuery(settings.tranformationSourceList)

    val deltaLink = shape.deltaLink
    val isShapeSource = deltaLink.isSource

    val possibleSource = p.shapes.filter { _.deltaLink.isSource }
    val sourceList = ("-1", "No Source") +: possibleSource.map(s => (s.deltaLink.key + "", s.name))
    setNamedOptionsInList(sourceList, tranformationSourceList)

    if (isShapeSource) {
      position.value("0")
      tranformationSourceList.value("-1")
    } else {
      position.value("" + deltaLink.position)
    }
    if (deltaListElemts.isEmpty) {
      deltaList.hide(500)
    } else {
      val source = if (isShapeSource)
        (shape, pos.layer)
      else deltaListElemts.find(_._1.deltaLink.slider == "None") match {
        case Some(s) =>
          tranformationSourceList.value("" + s._1.deltaLink.key)
          s
        case None => throw new Exception("PannelDelta : the delta group of " + shape.name + "have no source")
      }

      val src = ("" + source._2, source._1.name + "  (Source)")
      val params = deltaListElemts
        .filterNot(_._1.deltaLink.slider == "None")
        .sortBy(t => (t._1.deltaLink.slider, t._1.deltaLink.position))
        .map(t => ("" + t._2, t._1.name + "  (" + t._1.deltaLink.slider + " : " + t._1.deltaLink.position + ")"))
      setNamedOptionsInList(src +: params, deltaList)
      deltaList.value("" + pos.layer)
    }

    sliderBinding.value(deltaLink.slider)
    setOptionsInList("None" +: charmaker.sliders, sliderSuggestion)

  }

  def bind(callbacks: CallbackCenter, settings: CvSetting) {

    val position = jQuery(settings.tranformationPosition)
    position.on("input", changePosition(callbacks, position) _)

    val validate = jQuery(settings.bindDeltaButton)
    val sliderBinding = jQuery(settings.tranformationSliderSelect)
    val tranformationSourceList = jQuery(settings.tranformationSourceList)

    validate.click(validateBinding(callbacks, position, sliderBinding, tranformationSourceList) _)

    val deltaList = jQuery(settings.tranformationList)
    deltaList.change(deltaListChange(callbacks, deltaList) _)
  }

  private def deltaListChange(callbacks: CallbackCenter, list: JQuery)(evt: JQueryEventObject) = {
    val selected = list.value().toString().toInt
    callbacks.onLayerSelected(selected, SelectShapes)
  }
  private def changePosition(callbacks: CallbackCenter, input: JQuery)(evt: JQueryEventObject) = {
    val newPos = input.value().toString().toInt
    callbacks.onDeltaMovedOnSlider(newPos)
  }
  private def validateBinding(
    callbacks: CallbackCenter,
    positionIn: JQuery,
    sliderIn: JQuery,
    sourceIn: JQuery)(evt: JQueryEventObject) = {
    val pos = positionIn.value().toString().toInt
    val sliderName = sliderIn.value().toString()
    val sourceKey = sourceIn.value().toString().toInt
    if (sliderName.isEmpty() || sliderName == "???") {
      sliderIn.value("???")
    } else {
      val link = if (sourceKey < 0) {
        DeltaLink()
      } else {
        new DeltaLink(sourceKey,sliderName,pos)
      }
      callbacks.onShapeDeltaLinkChanged(link)
    }

  }
}