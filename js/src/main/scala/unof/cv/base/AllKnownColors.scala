package unof.cv.base

import scala.scalajs.js.Dynamic
import scala.scalajs.js
import scala.util.Random
import scala.scalajs.js.Any.jsArrayOps

object AllKnownColors {
  val list = Dynamic.global.allColors.asInstanceOf[js.Array[ColorIdent]].toSeq
  /**
   * code with # before
   */
  val mapName = list.map(c =>(c.simpleName,c.code)).toMap
  val cssOnly = Set("AliceBlue","AntiqueWhite","Aqua","Aquamarine","Azure","Beige","Bisque","Black","BlanchedAlmond","Blue","BlueViolet","Brown","BurlyWood","CadetBlue","Chartreuse","Chocolate","Coral","CornflowerBlue","Cornsilk","Crimson","Cyan","DarkBlue","DarkCyan","DarkGoldenRod","DarkGray","DarkGrey","DarkGreen","DarkKhaki","DarkMagenta","DarkOliveGreen","Darkorange","DarkOrchid","DarkRed","DarkSalmon","DarkSeaGreen","DarkSlateBlue","DarkSlateGray","DarkSlateGrey","DarkTurquoise","DarkViolet","DeepPink","DeepSkyBlue","DimGray","DimGrey","DodgerBlue","FireBrick","FloralWhite","ForestGreen","Fuchsia","Gainsboro","GhostWhite","Gold","GoldenRod","Gray","Grey","Green","GreenYellow","HoneyDew","HotPink","IndianRed","Indigo","Ivory","Khaki","Lavender","LavenderBlush","LawnGreen","LemonChiffon","LightBlue","LightCoral","LightCyan","LightGoldenRodYellow","LightGray","LightGrey","LightGreen","LightPink","LightSalmon","LightSeaGreen","LightSkyBlue","LightSlateGray","LightSlateGrey","LightSteelBlue","LightYellow","Lime","LimeGreen","Linen","Magenta","Maroon","MediumAquaMarine","MediumBlue","MediumOrchid","MediumPurple","MediumSeaGreen","MediumSlateBlue","MediumSpringGreen","MediumTurquoise","MediumVioletRed","MidnightBlue","MintCream","MistyRose","Moccasin","NavajoWhite","Navy","OldLace","Olive","OliveDrab","Orange","OrangeRed","Orchid","PaleGoldenRod","PaleGreen","PaleTurquoise","PaleVioletRed","PapayaWhip","PeachPuff","Peru","Pink","Plum","PowderBlue","Purple","Red","RosyBrown","RoyalBlue","SaddleBrown","Salmon","SandyBrown","SeaGreen","SeaShell","Sienna","Silver","SkyBlue","SlateBlue","SlateGray","SlateGrey","Snow","SpringGreen","SteelBlue","Tan","Teal","Thistle","Tomato","Turquoise","Violet","Wheat","White","WhiteSmoke","Yellow","YellowGreen");
  private def cleanName(colorName:String) = colorName.filter { _.isLetterOrDigit }.toLowerCase()
  def colorThis(colorName : String) : String = getOrFindSomethingClose(colorName)match {
    case Some(c)=> c
    case None =>"#000000";
  }
  def getOrFindSomethingClose(colorName : String):Option[String]= {
    val clean = cleanName(colorName)
    
    def superColor = {
      mapName.toSet.filter { _._1.contains(clean) }
          .toSeq.sortBy(_._1.length())
    }
    def subColor = {
      mapName.toSet.filter {t=> clean.contains(t._1) }
              .toSeq.sortBy(-_._1.length())
    }
    if(clean.startsWith("#") && isColorCode(clean.tail))
      Some(clean)
    else if(isColorCode(clean))
      Some("#"+clean)
    else
    mapName.get(clean) match {
      case Some(code)=> Some(code)
      case None => 
        val candidates ={
          val superC = superColor
          if(superC.isEmpty){
            
            if(colorName.exists {!_.isLetterOrDigit}){
              val split = colorName.split("[^\\d\\w]+")
              val composingColor = (split map getOrFindSomethingClose)
                .filter(_.isDefined)
                .map(_.get)
              if( composingColor.isEmpty){
                Nil
              }else{
                val colorCount = composingColor.size
                val fffCol = composingColor
                .map{toFloat3}
                .reduce[(Float,Float,Float)]{
                  case ((a,b,c),(d,e,f))=>(a+d,b+e,c+f)
                }
                Seq(("","#"+toHexaString((fffCol._1/colorCount,fffCol._2/colorCount,fffCol._3/colorCount))))
              }
              
            }else {
              val subC = subColor
              if(subC.isEmpty){
                Nil
              }else{
                subC
              }
            }
          }else{
            superC
          }
        }
          
        if(candidates.isEmpty)
          None
        else{
          val bestCandidates = candidates.takeWhile(_._1.length() == candidates.head._1.length())
            
          Some(bestCandidates((math.random * bestCandidates.size).toInt)._2)
        }
        
    }
  }
  def isColorCode(s:String)={
    s.forall { 
      c =>
        c>='A' && c <='F' || c >='0' && c <='9' || c >= 'a' && c <='f'
    }
  }
  def get(colorName : String)= {
    mapName.get(cleanName(colorName))
  }
  def randomColor(r:Random) = {
    list(r.nextInt(list.size))
  }
  def randomName(r:Random) = {
    list(r.nextInt(list.size)).name
  }
  def randomSimpleName(r:Random) = {
    list(r.nextInt(list.size)).simpleName
  }
  def randomCSSName(r:Random) = {
    cssOnly.toSeq(r.nextInt(cssOnly.size))
  }
  def toCSSValideColor(color : String) = {
    if(cssOnly.contains(color))
      color
    else {
      "#"+mapName(cleanName(color))
    }
  }
  def toFloat3(color : String)={
    val trimedColor = color.trim
    val cleanColor = (if(trimedColor.charAt(0)=='#') color.drop(1) else trimedColor)
    
    val seq = cleanColor.grouped(2) map( s=>Integer.parseInt(s,16)/255f)
    (seq.next(),seq.next(),seq.next())
  }
  def toCode(colorName : String) = {
    mapName(cleanName(colorName))
  }
  def toHexaString(color : (Float,Float,Float))={
    def hexa(f:Float) = (255 * f).intValue().toHexString.reverse.padTo(2, '0').reverse
    hexa(color._1)+hexa(color._2)+hexa(color._3)
  }
}