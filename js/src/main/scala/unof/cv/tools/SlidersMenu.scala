package unof.cv.tools

import org.scalajs.jquery.jQuery
import org.scalajs.jquery.JQuery
import org.scalajs.jquery.JQueryEventObject
import unof.cv.base.charLib.CMShape
import unof.cv.base.charLib.DeltaLink

object SlidersMenu {
  def create(callbacks: CallbackCenter, settings: CvSetting) = {
    val sliderDivName = settings.slidersDiv
    val slideCount = settings.maxSliders.intValue()
    val slideDiv = jQuery(sliderDivName)
    (0 to slideCount - 1) foreach {
      i =>
        val slideAlias = "sDiv" + i
        val slidePersoDiv = jQuery("<div id=\"span" + slideAlias + "\" class=\"popupElem\">")
        val slideNameSpan = jQuery("<span id=\"panpan" + slideAlias + "\">")
        val slideElem = jQuery("<input type=\"range\" name=\"" + slideAlias + "\" id=\"" + slideAlias + "\" step=\"1\">")
        val slideValuSpan = jQuery("<span id=\"valueOf" + slideAlias + "\">")
        slideElem.on("input", sliderChanged(callbacks, i, slideElem) _)
        slidePersoDiv.append(slideNameSpan)
        slidePersoDiv.append(slideElem)
        slidePersoDiv.append(slideValuSpan)
        slideDiv.append(slidePersoDiv)
    }

  }
  def update(callbacks: CallbackCenter, setting: CvSetting) = {
    val selectedDelta =
      if (callbacks.selection.category > 0)
        callbacks.selection.mapSelected(
          callbacks.charMaker,
          (i) => DeltaLink(), (s) => s.deltaLink, (p) => DeltaLink(), (c) => DeltaLink())
      else
        DeltaLink()
    val slidesDivName = setting.slidersDiv
    val slidesDiv = jQuery(slidesDivName)
    var rangeMap = Map[String, (Int, Int)]()

    def checkShape(s: CMShape): Unit = {
      val d = s.deltaLink
      if (d.slider != "None") {
        val oldEntry = rangeMap.get(d.slider) match {
          case None =>
            (0, 0)
          case Some(t) =>
            t
        }
        rangeMap += d.slider -> ((oldEntry._1 min d.position, oldEntry._2 max d.position))
      }
    }
    callbacks.charMaker.categories foreach {
      _.possibleParts foreach {
        _.shapes.foreach(checkShape)
      }
    }
    val newSlidesName = callbacks.charMaker.sliders
    val newSlidersValues = callbacks.slidersValues

    if (newSlidesName.isEmpty || newSlidersValues.isEmpty) {
      (0 to setting.colorSlotsCount.intValue() - 1) foreach {
        i => jQuery("#spansDiv" + i).hide(500)
      }
    } else {
      (newSlidesName.zip(newSlidersValues)).zipWithIndex foreach {
        case ((name, value), i) =>
          val sliderAlias = "sDiv" + i
          val slideSpan = jQuery("#span" + sliderAlias)
          slideSpan.show(500)
          val colorNameSpan = jQuery("#panpan" + sliderAlias)
          colorNameSpan.empty()
          colorNameSpan.append(name + " : ")
          val sliderElem = jQuery("#" + sliderAlias)

          sliderElem.attr("disabled", false)
          sliderElem.value(value.toString())
          val (min, max) = rangeMap(name)
          sliderElem.prop("min", min)
          sliderElem.prop("max", max)

          val slideValuSpan = jQuery("#valueOf" + sliderAlias)
          slideValuSpan.empty()
          slideValuSpan.append(value.toString())

          if (name == selectedDelta.slider) {
            sliderElem.value(selectedDelta.position.toString())
            sliderElem.attr("disabled", true)
          }
      }
      (newSlidesName.size to setting.maxSliders.intValue() - 1) foreach {
        i => jQuery("#spansDiv" + i).hide(500)
      }
    }
  }

  private def sliderChanged(callbacks: CallbackCenter, index: Int, input: JQuery)(evt: JQueryEventObject) = {
    val v = input.value().toString().toInt
    val sliderAlias = "sDiv" + index
    val slideValuSpan = jQuery("#valueOf" + sliderAlias)
    slideValuSpan.empty()
    slideValuSpan.append(v.toString())
    callbacks.setShapeSelected(false)
    callbacks.onSliderChange(index, v)
  }
}