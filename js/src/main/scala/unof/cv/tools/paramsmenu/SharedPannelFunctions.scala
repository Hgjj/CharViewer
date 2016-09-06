package unof.cv.tools.paramsmenu

import org.scalajs.jquery.jQuery
import org.scalajs.jquery.JQuery
import unof.cv.base.charmaker.CharMaker
import unof.cv.base.charmaker.CMLayer
import unof.cv.base.charmaker.CMPart
import unof.cv.base.Transforme
import org.scalajs.jquery.JQueryEventObject
import unof.cv.base.charmaker.CMShape
import unof.cv.base.charmaker.CMCategory
import scala.scalajs.js.Any.fromString
import unof.cv.base.charmaker.CMAdress.toT4
import unof.cv.tools.CallbackCenter

trait SharedPannelFunctions {
  protected def syncColor(variableName: String, thingToSync: JQuery, options: CharMaker) = {

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
  protected def setOptionsInList(options: Seq[String], list: JQuery) {
    list.empty()
    options.foreach {
      c => list.append("<option value=\"" + c + "\">" + c + "</option>")
    }
  }
  protected def setNamedOptionsInList(options: Seq[(String,String)], list: JQuery) {
    list.empty()
    options.foreach {
      t => 
        list.append("<option value=\"" + t._1 + "\">" + t._2 + "</option>")
    }
  }
  
  protected def getTrParamInCPI(callbacks: CallbackCenter, f: (Transforme) => Double) = {
    callbacks.selection.mapSelected(
      callbacks.charMaker,
      (l : CMLayer) => f(l.transform),
      (p : CMPart) => f(p.partTransform),
      (c:CMCategory) => comonPValue(callbacks, 0.0, c => f(c.partTransform))
      )
  }
  protected def comonValue[A](default: A, f: (CMLayer) => A, selectedPart: CMPart): A = {
    val existings = selectedPart.components.map(f)
    if (existings.tail.forall { _ == existings.head }) {
      existings.head
    } else
      default
  }
  protected def comonPValue[A](callbacks: CallbackCenter, default: A, f: (CMPart) => A): A = {
    val selectedCategory = callbacks.currentOptions.categories(callbacks.selection._1)
    val existings = selectedCategory.possibleParts.map(f)
    if (existings.tail.forall { _ == existings.head }) {
      existings.head
    } else
      default
  }
  protected def getSource(sourceIn: Any) = {
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
  protected def onTransformChange(
    callbacks: CallbackCenter,
    change: (Transforme, Float) => (Transforme, Float))(evt: JQueryEventObject) = {
    callbacks.onImageTransformed(change(_, _))
  }

  protected implicit def jqObj2Numb(jq: JQuery) = try {
    jq.value.toString().toDouble
  } catch {
    case _: Throwable => 0.0
  }
  protected def onlyForShape(f :(CMShape)=>CMLayer) : (CMLayer)=>CMLayer = {
    _ match {
      case shape : CMShape => f(shape)
      case other => throw new UnsupportedOperationException("That can only be done on shape, not on "+other)
    }
      
  }
  
  
}