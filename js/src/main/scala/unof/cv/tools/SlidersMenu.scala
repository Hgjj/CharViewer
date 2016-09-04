package unof.cv.tools

import org.scalajs.jquery.jQuery
import org.scalajs.jquery.JQuery
import org.scalajs.jquery.JQueryEventObject
import unof.cv.base.charmaker.CMShape

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

        slideElem.on("input",sliderChanged(callbacks, i, slideElem) _)
        slidePersoDiv.append(slideNameSpan)
        slidePersoDiv.append(slideElem)
        slideDiv.append(slidePersoDiv)
    }

  }
  def update(callbacks: CallbackCenter, setting: CvSetting) = {
    val slidesDivName = setting.slidersDiv
    val slidesDiv = jQuery(slidesDivName)
    var rangeMap = Map[String, (Int, Int)]()
    
    def checkShape(s: CMShape): Unit = {
      s.deltas.foreach {
        slide =>
          slide._2.foreach {
            t =>
              rangeMap.get(slide._1) match {
                case None =>
                  val entry = (0 min t._1, 0 max t._1)
                  rangeMap += slide._1 -> entry
                case Some((in, ax)) =>
                  val entry = (in min t._1, ax max t._1)
                  rangeMap += slide._1 -> entry
              }
              checkShape(t._2)
          }
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
          val colorSpan = jQuery("#span" + sliderAlias)
          colorSpan.show(500)
          val colorNameSpan = jQuery("#panpan" + sliderAlias)
          colorNameSpan.empty()
          colorNameSpan.append(name + " : ")
          val sliderElem = jQuery("#" + sliderAlias)
          sliderElem.value(value.toString())
          val (min,max) = rangeMap(name)
          sliderElem.prop("min",min)
          sliderElem.prop("max",max)
      }
      (newSlidesName.size to setting.maxSliders.intValue() - 1) foreach {
        i => jQuery("#spansDiv" + i).hide(500)
      }
    }
  }

  private def sliderChanged(callbacks: CallbackCenter, index: Int, input: JQuery)(evt: JQueryEventObject) = {
    val v = input.value().toString().toInt
    callbacks.setShapeSelected(false)
    callbacks.onSliderChange(index, v)
  }
}