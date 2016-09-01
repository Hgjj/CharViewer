package unof.cv.tools

import unof.cv.base.Algebra._
import org.scalajs.jquery.jQuery
import scala.util.Random
import org.scalajs.jquery.JQueryEventObject
import org.scalajs.jquery.JQuery
import scala.scalajs.js.Dynamic.{global => g}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import unof.cv.base.AllKnownColors
import unof.cv.base.charmaker.CharMaker
object DrawMenu {

  val partNameFontSize: Int = 20
  val partNameFont: String = partNameFontSize + "px Arial "
  val partNameColor: String = "black"
  val arrowColor: String = "black"
  val partChooserInnerGap: Int = 3
  val betweenChooserGap: Int = 5
  val beforChooserGap: Int = 3
  val arrowHalphBase: Int = 13
  val arrowLength: Int = 22
  val arrowBoxSide: Int = 24
  val r = new Random(51216543)
  private var classAlias: Map[String, String] = Map()
  def getAlias(elemName: String) = {
    classAlias.get(elemName) match {
      case Some(alias) => alias
      case None =>
        val newAlias = "viewerClass" + classAlias.size
        classAlias += (elemName -> newAlias)
        newAlias
    }
  }

  def updateMenu(oldCM: CharMaker, newCM: CharMaker, oldChoices : Seq[Int], newChoices : Seq[Int], callbacks: CallbackCenter, settings: CvSetting) = {
     
    
    val onPartInMenuClicked = callbacks.onPartInMenuClicked _
    val slide = jQuery(settings.categoriesTabContainer)
    val oldCategories = oldCM.categories map (_.categoryName)
    val newCategories = newCM.categories map (_.categoryName)
    val oldPartsName = oldCM.categories
      .map(_.possibleParts map (_.partName))
    val newPartsName = newCM.categories
      .map(_.possibleParts map (_.partName))
    val removedCategories = oldCategories.toSet -- newCategories
    val addedCategories = newCategories.toSet -- oldCategories
    val oldPartMap = oldCategories.zip(oldPartsName).toMap.withDefault(x=>Nil)
    val newPartMap = newCategories.zip(newPartsName).toMap
    val oldParts = newCategories.flatMap(cat => oldPartMap(cat).map(cat + "_" + _)).toSet
    val newParts = newCategories.zip(newPartsName).flatMap(t => t._2.map(t._1 + "_" + _)).toSet
    val removedParts = oldParts -- newParts
    val addedPart = newParts -- oldParts

    (removedCategories ++ removedParts).foreach {
      s => jQuery("." + getAlias(s)).remove()
    }
    newCategories.zipWithIndex.foreach {
      case (cat, i) =>
        if (!oldCategories.contains(cat)){
          addCategory(cat, "", Nil, i, onPartInMenuClicked, r, slide)
        }
          
    }
    addedPart foreach {
      p =>
        val Array(cat, part) = p.split("_")
        val catA = jQuery(".a_" + getAlias(cat))
        addAllParts(Seq(part), cat, "", onPartInMenuClicked, catA)
    }
    def setAsContent(
      partSelection: Seq[Int],
      partsName: Seq[Seq[String]],
      categories: Seq[String])(newA: (String) => String) ={
      (partSelection zip partsName)
        .map(t => t._2(t._1))
        .zip(categories)
        .foreach {
        
          case (part, cat) =>
             val partElem = jQuery("." + getAlias(cat + "_" + part))
            if (partElem.length != 0) {
              val a = partElem.children("a")
              a.empty()
              a.append(newA(part))
            }
        }
    }
    setAsContent(oldChoices, oldPartsName, oldCategories) {
      part => part
    }
    setAsContent(newChoices, newPartsName, newCategories) {
      part => "<mark>" + part + "</mark>"
    }
    

  }
  private def addPart(part: String, selected: String, category: String, onPartInMenuClicked: (String, String, JQueryEventObject) => Any, to: JQuery) = {

    val decoPart = if (part == selected) {
      "<mark>" + part + "</mark>"
    } else
      part
    val jqPart = jQuery("<dd class=\"" + getAlias(category + "_" + part) + "\"><a>" + decoPart + "</a></dd>")
    jqPart.click(onPartInMenuClicked(category, part, _: JQueryEventObject))
    to.append(jqPart)
  }
  private def addAllParts(parts: Seq[String], category: String, selected: String, onPartInMenuClicked: (String, String, JQueryEventObject) => Any, to: JQuery) = {
    val subs = getOrAdd(
      ".subs" + getAlias(category),
      "<div class=\"subs subs" + getAlias(category) + " " + getAlias(category) + "\">",
      to)

    def pushInDls(unpushedParts: Seq[String], dlIndex: Int = 0): Unit =
      if (!unpushedParts.isEmpty) {
        val dl = getOrAdd(
          "." + getAlias(category + "_" + dlIndex),
          "<dl class=\"" + getAlias(category + "_" + dlIndex) + "\">",
          subs)
        val space = 4 - dl.children().length
        val (pushHere, pushLater) = unpushedParts.splitAt(space)
        pushHere.foreach { addPart(_, selected, category, onPartInMenuClicked, dl) }

        pushInDls(pushLater, dlIndex + 1)
      }

    pushInDls(parts)
  }
  private def addCategory(
    category: String,
    selected: String,
    parts: Seq[String],
    categoryIndex: Int,
    onPartInMenuClicked: (String, String, JQueryEventObject) => Any,
    r: Random,
    to: JQuery) = {
    val title = category.head.toUpper + category.tail
    val li = jQuery("<li>")
    val color = AllKnownColors.randomColor(r)
    val a = jQuery("<a class=\"" +
      getAlias(category) +
      " a_" +
      getAlias(category) +
      "\">")
    a.click(onPartInMenuClicked(category,"",_ : JQueryEventObject))
    a.css("background-color", color.code)
    a.append(title)
    li.append(a)
    addAllParts(parts, category, selected, onPartInMenuClicked, li)

    to.append(li)
  }
  private def getOrAdd(selector: String, full: String, parent: JQuery) = {
    val oldStuff = jQuery(selector)
    if (oldStuff.length == 0) {
      val newStuff = jQuery(full)
      parent.append(newStuff)
      newStuff
    } else {
      oldStuff
    }

  }
  def createMenu(settings: CvSetting, callbacks: CallbackCenter) = {
    
    val onPartInMenuClicked = callbacks.onPartInMenuClicked _

    val slide = jQuery(settings.categoriesTabContainer)
    callbacks.currentOptions.categories.zip(callbacks.currentChoices)
      .zipWithIndex
      .foreach {
        case ((category, slected), categoryIndex) =>
          addCategory(
            category.categoryName,
            category.possibleParts(slected).partName,
            category.possibleParts.map(_.partName),
            categoryIndex,
            onPartInMenuClicked,
            r,
            slide)

      }
    
    
  }
 
    
    
}
