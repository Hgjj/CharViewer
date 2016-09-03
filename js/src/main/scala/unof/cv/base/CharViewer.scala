package unof.cv.base

import scala.scalajs.js
import scala.scalajs.js.Date
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.Any.wrapArray
import scala.scalajs.js.Any.wrapDictionary
import unof.cv.base.charmaker.CharMaker



@JSExport
class CharViewer(bodyParts : js.Array[JsBodyPart], val targetCanvas : String, val imageHome : String){
   val charMaker = CharMaker(bodyParts,false)
   
   var choices : Seq[Int] = Seq.fill(charMaker.categories.size)(0)
   var colorMask : Seq[String] = Seq.fill(charMaker.colors.size)("white")
   var slidersValues : Seq[Int] = Seq.fill(charMaker.sliders.size)(0)
  
   @JSExport
   def drawChar() ={
     println("CharViewer drawChar "+choices.mkString(", "))
     charMaker.makeChar(choices, colorMask,slidersValues,Seq(Transforme()),imageHome){
       c => 
       val drawOn = new DrawingContext(targetCanvas)
       DrawChar(c,drawOn)
   
     }
   }
     
  @JSExport
  def choose(category : String, part : String): Unit = {
    val (catIndex,partIndex)  = charMaker.getLocation(category, part)
    if(partIndex >=0){
      choices = choices.updated(catIndex, partIndex)
    }else {
      println("Unable to find the part "+part+" in "+category)
    }
  }
  @JSExport
  def chooseAll(dic : js.Dictionary[String]): Unit = {
    val choiceSeq = dic.toSeq
    choiceSeq.foreach(t=>choose(t._1, t._2))
  }
  @JSExport
  def chooseAllColors(dic : js.Dictionary[String]):Unit = {
    val choiceSeq = dic.toSeq
    choiceSeq.foreach(t=>chooseColor(t._1, t._2))
  }
  @JSExport
  def chooseColor(variableName : String, choosenColor : String):Unit = {
    def isHexaChar(c:Char) = c >= '0' && c <='9' ||
      c >='a' && c <='f' ||
      c >='A' && c <='F'
    val colorValue = if(choosenColor(0) =='#')
      choosenColor
    else if(choosenColor.length() == 6 && choosenColor.forall(isHexaChar) ){
      "#"+choosenColor
    } else
      AllKnownColors.getOrFindSomethingClose(choosenColor) match {
        case Some(code) => code
        case None => 
          val d = Date.now()
          val i = d.toString().reverse.take(8).toInt
          val t = i % AllKnownColors.list.size
          val choosenByMe = AllKnownColors.list(t)
          println("I don't know the color "+choosenColor+", so what about this beautifull "+choosenByMe.name+" ?")
          choosenByMe.code
      }
      
    val targetIndex = charMaker.colors.indexOf(variableName)
    if(targetIndex >= 0){
      colorMask = colorMask.updated(targetIndex, colorValue)
    }else{
      println("I don't have any color variable named "+variableName)
    }
  }
}