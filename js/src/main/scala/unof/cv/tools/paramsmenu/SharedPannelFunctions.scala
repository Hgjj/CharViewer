package unof.cv.tools.paramsmenu

import scala.scalajs.js.Any.fromString

import org.scalajs.jquery.JQuery
import org.scalajs.jquery.JQueryEventObject
import org.scalajs.jquery.jQuery

import unof.cv.utils.Transforme
import unof.cv.base.charLib.CMAdress.toT4
import unof.cv.base.charLib.CMCategory
import unof.cv.base.charLib.CMLayer
import unof.cv.base.charLib.CMPart
import unof.cv.base.charLib.CharacterLibrary
import unof.cv.tools.CallbackCenter

object SharedPannelFunctions {
  def syncColor(variableName: String, thingToSync: JQuery, options: CharacterLibrary) = {

    thingToSync.`val`(variableName)
    if (variableName != "None" && variableName != "") {
      val colorSelector = jQuery("#cDiv" + options.colors.indexOf(variableName))
      try {

        val fontColor = colorSelector.css("color")
        val backgroundColor = colorSelector.css("background-color")
        thingToSync.css("color", fontColor)
        thingToSync.css("background-color", backgroundColor)
      } catch {
        case _: Throwable =>
          thingToSync.css("color", "rgb(0, 0, 0)")
          thingToSync.css("background-color", "rgb(255, 255, 255)")
      }

    } else {
      thingToSync.css("color", "rgb(0, 0, 0)")
      thingToSync.css("background-color", "rgb(255, 255, 255)")
    }
  }
  def setOptionsInList(options: Seq[String], list: JQuery) {
    list.empty()
    options.foreach {
      c => list.append("<option value=\"" + c + "\">" + c + "</option>")
    }
  }
  def setNamedOptionsInList(options: Seq[(String, String)], list: JQuery) {
    list.empty()
    options.foreach {
      t =>
        list.append("<option value=\"" + t._1 + "\">" + t._2 + "</option>")
    }
  }

  def getTrParamInCPI(callbacks: CallbackCenter, f: (Transforme) => Double): String = {
    callbacks.selection.mapSelected(
      callbacks.charMaker,
      (l: CMLayer) => f(l.transform).toString(),
      (p: CMPart) => f(p.partTransform).toString(),
      (c: CMCategory) => comonPValue(callbacks, "0.0", c => f(c.partTransform).toString()))
  }
  def comonValue(default: Any, f: (CMLayer) => Any, selectedPart: CMPart): String = {
    val existings = selectedPart.components.map(f)
    if (existings.tail.forall { _ == existings.head }) {
      existings.head.toString()
    } else
      default.toString()
  }
  def comonPValue(callbacks: CallbackCenter, default: Any, f: (CMPart) => Any): String = {
    val selectedCategory: CMCategory = callbacks.currentOptions.categories(callbacks.selection._1)
    val ps : Seq[CMPart] = selectedCategory.possibleParts
    val existings:Seq[Any] = ps.map(f)
    def isComon (a:Any) = {
      a == existings.head
    }
    if (existings.tail.forall(isComon)) {
      existings.head.toString()
    } else
      default.toString()
  }
  def getSource(sourceIn: Any): String = {
    val rawSource = sourceIn.toString().trim

    if (rawSource.isEmpty() || rawSource == "None")
      "None"
    else {
      val split = rawSource.split("[\\\\/]")
      if (split.head == "images") {
        split.tail.mkString("/")
      } else {
        split.mkString("/")
      }
    }
  }
  def onTransformChange(
    callbacks: CallbackCenter,
    change: (Transforme, Float) => (Transforme, Float))(evt: JQueryEventObject): Unit = {
    callbacks.onImageTransformed(change)
  }

}