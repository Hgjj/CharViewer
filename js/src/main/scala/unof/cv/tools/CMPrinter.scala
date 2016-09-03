package unof.cv.tools

import scala.scalajs.js.Any.fromFunction0
import scala.scalajs.js.Any.fromInt
import scala.scalajs.js.Any.fromString
import scala.scalajs.js.Date
import scala.scalajs.js.Dynamic
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.URIUtils

import unof.cv.base.Algebra.Vec
import unof.cv.base.charmaker.BoundColor
import unof.cv.base.charmaker.CMImage
import unof.cv.base.charmaker.CMLayer
import unof.cv.base.charmaker.CMPart
import unof.cv.base.charmaker.CMShape
import unof.cv.base.charmaker.ConstantColor
import unof.cv.base.charmaker.CurveTo
import unof.cv.base.charmaker.DynamicColor
import unof.cv.base.charmaker.LinkedVisibility
import unof.cv.base.charmaker.MoveTo
import unof.cv.base.charmaker.SelectImages
import unof.cv.base.charmaker.VisibilityCondition

object CMPrinter {

  private var lastSave: Double = 0;
  def print(fileName: String, state: AppStat) = {
    val save = all(state, "var fileCMParams")
    download(fileName, save)

  }
  def makePeriodicalCookieSaves(periode: Int, state: AppStat) = {
    if (periode > 0) {
      Dynamic.global.setInterval(periodicalCookieSave(periode, state), periode)
    }
  }
  def periodicalCookieSave(periode: Int, state: AppStat) = () => {
    val now = Date.now
    val lastChange = state.lastEddition
    if (now - lastChange > periode && lastChange != lastSave) {
      lastSave = lastChange
      cookie(state)
    }
  }
  def cookie(state: AppStat) = {
    val save = LWZ.compress(all(state, "cookieCMParams")
      .filterNot { _.isWhitespace })
      .map(_.toChar)
      .mkString
    Dynamic.global.localStorage.setItem( "cookieCMParams",save )
  }
  private def all(

    state: AppStat,
    structName: String): String = {

    val cm = state.charMaker
    val colors = state.colorMask
    val choices = state.choices
    val selected = Seq(
      state.selection.category,
      state.selection.part,
      state.selection.layer,
      if (state.selection.layerSelect == SelectImages)
        0
      else
        1)

    def format(s: Seq[Any]) = s.mkString("[ ", ", ", " ]")
    val partFields = Seq(
      "partName",
      "category",
      "partZ",
      "partTransform",
      "components")
    val imageFields = Seq(
      "imageRef",
      "transform",
      "colorVariable",
      "z_layer",
      "condition",
      "name")

    val shapeFields = Seq(
      "points",
      "transform",
      "colorVariables",
      "z_layer",
      "condition",
      "lineWidth",
      "showSurface",
      "lineJoin",
      "closed",
      "deltas",
      "linkedSlider",
      "name")
    def assembleStruct(parNames: Seq[String], parValues: Seq[Any]) = {
      parNames.zip(parValues)
        .map {
          case (fName, fValue) => "\"" + fName + "\" : " + fValue
        }.mkString("\t\t\t{", ",\n\t\t\t\t", "\n\t\t\t}")
    }

    def oneLayer(l: CMLayer) = {
      l match {
        case img: CMImage => oneImage(img)
        case s: CMShape   => oneShape(s)
      }
    }
    def oneVec(v: Vec) = "{ \"x\" : " + v._1 + ", \"y\" : " + v._2 + " }"
    def oneDynamicColor(d: DynamicColor) = "\"" + (d match {
      case c: ConstantColor => "C(" + c.alpha + ")_" + c.value.map(escapeEnoyingChar).mkString
      case b: BoundColor    => "V(" + b.alpha + ")_" + b.value.map(escapeEnoyingChar).mkString
    }) + "\""
    def oneCondition(c: VisibilityCondition) = (c match {
      case LinkedVisibility(key) =>
        val (theCat, thePart) = cm.linkKeyMap(key)
        Seq(LinkedVisibility.key, theCat, thePart)
      case other => Seq(other.key)
    }).mkString("[ \"", "\", \"", "\" ]")
    def oneShape(s: CMShape): String = {
      val (sliders, deltas) = s.deltas.toSeq.unzip
      val deltaString = format(deltas.map {
        s =>
          format(s.map {
            case (pos, shape) =>
              "{\"sliderPos\" : " + pos + ", \"state\" : " + oneShape(shape).replaceAll("\n", "") + "}"
          })
      })
      val values = Seq(
        format(s.commands.map {
          c =>
            format((c match {
              case ct: CurveTo =>
                Seq(ct.cp1, ct.cp2, ct.end)
              case mt: MoveTo =>
                Seq(mt.pos)
            }).map(oneVec))

        }),
        s.transform.toString(),
        format(s.colors.map(oneDynamicColor)),
        s.z,
        oneCondition(s.displayCondition),
        s.lineWidth,
        s.showSurcface,
        "\"" + s.lineJoint + "\"",
        s.closed,
        deltaString,
        format(sliders.map("\"" + _.map(escapeEnoyingChar).mkString + "\"")),
        "\"" + s.name.map(escapeEnoyingChar).mkString + "\"")
      assembleStruct(shapeFields, values)
    }
    def oneImage(img: CMImage): String = {

      val values = Seq(
        "\"" + cm.imageMap(img.ref).hRef + "\"",
        img.transform.toString(),
        "\"" + img.boundColor.map(escapeEnoyingChar).mkString + "\"",
        img.z,
        oneCondition(img.displayCondition),
        "\"" + img.name.map(escapeEnoyingChar).mkString + "\"")
      assembleStruct(imageFields, values)
    }

    def onePart(catName: String, part: CMPart) = {
      val values = Seq(
        "\"" + part.partName.map(escapeEnoyingChar).mkString + "\"",
        "\"" + catName.map(escapeEnoyingChar).mkString + "\"",
        part.partZ,
        part.partTransform.toString(),
        part.components.map(oneLayer).mkString("[\n", ",\n", "\n\t\t]"))
      values.zip(partFields).map {
        t => "\"" + t._2 + "\" : " + t._1
      }.mkString("\n\t{ ", ",\n\t\t ", " \n\t}")
    }

    def bodyParts =
      cm.categories
        .flatMap(c => c.possibleParts.map((onePart(c.categoryName, _))))
        .mkString("\"bodyParts\" : [", ",\n", "\n]")
    def colorsMask = "\"colors\" : " + format(colors.map("\"" + _ + "\""))
    def savedChoices = "\"choices\" : " + format(choices)
    def savedSelected = "\"selected\" : " + format(selected)
    def savedDate = "\"date\" : " + Date.now
    Seq(bodyParts, colorsMask, savedChoices, savedSelected, savedDate)
      .mkString(structName + " = {", ",\n", "\n};")
  }

  private def escapeEnoyingChar(c: Char): String = c match {
    case '\n' => "\\n"
    case '\"' => "'"
    case '\r' => "\\r"
    case '\\' => "/"
    case c    => "" + c
  }
  private def download(filename: String, text: String) {
    println("1")
    var element = Dynamic.global.document.createElement("a");
    println("a")
    element.setAttribute("href", "data:text/plain;charset=utf-8," + URIUtils.encodeURIComponent(text));
    println("b")
    element.setAttribute("download", filename);
    println("c")
    element.style.display = "none";
    global.document.body.appendChild(element);

    element.click();

    global.document.body.removeChild(element);
  }
}