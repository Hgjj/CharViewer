package unof.cv.base.charmaker

import unof.cv.base.Transforme
import unof.cv.base.Algebra._
import unof.cv.base.AllKnownColors
import java.util.regex.Pattern

class CMShape(
    val commands: Seq[DrawCommand],
    val transform: Transforme,
    val colors: Seq[DynamicColor],
    val z: Float,
    val displayCondition: VisibilityCondition,
    val lineWidth: Int,
    val showSurcface: Boolean,
    val lineJoint: String,
    val closed : Boolean,
    val deltas : Seq[(String,Seq[(Int,CMShape)])],
    val name : String) extends CMLayer {
  
  
  val lineColorIndex = 0
  val surfaceColorIndex = 1
  def boundColors = colors.flatMap {
    case ConstantColor(_)     => Nil
    case BoundColor(variable) => variable :: Nil
  }
  def setZ(newZ: Float)=
    new CMShape(
        commands,
        transform,
        colors,
        newZ,
        displayCondition,
        lineWidth,
        showSurcface,
        lineJoint,
        closed,
        deltas,
        name
    )
  def setColorBonds(newColor: Seq[String]) = {
    new CMShape(
        commands,
        transform,
        newColor.map(DynamicColor.apply(_, 1f)),
        z,
        displayCondition,
        lineWidth,
        showSurcface,
        lineJoint,
        closed,
        deltas,
        name
    )
  }

  def setColors(newColor: Seq[DynamicColor])= {
    new CMShape(
        commands,
        transform,
        newColor,
        z,
        displayCondition,
        lineWidth,
        showSurcface,
        lineJoint,
        closed,
        deltas,
        name
    )
  }

  def setCondition(newCondition: VisibilityCondition) =
    new CMShape(
        commands,
        transform,
        colors,
        z,
        newCondition,
        lineWidth,
        showSurcface,
        lineJoint,
        closed,
        deltas,
        name
    )
  def setTransform(newTransform: Transforme) =
    new CMShape(
        commands,
        newTransform,
        colors,
        z,
        displayCondition,
        lineWidth,
        showSurcface,
        lineJoint,
        closed,
        deltas,
        name
    )
  def setDrawCommands(newDrawCommands: Seq[DrawCommand]) =
    new CMShape(
        newDrawCommands,
        transform,
        colors,
        z,
        displayCondition,
        lineWidth,
        showSurcface,
        lineJoint,
        closed,
        deltas,
        name
    )
  def setDrawCommand(newDrawCommand: DrawCommand, curvindex  :Int) =
    new CMShape(
        commands.updated(curvindex, newDrawCommand),
        transform,
        colors,
        z,
        displayCondition,
        lineWidth,
        showSurcface,
        lineJoint,
        closed,
        deltas,
        name
    )
 
  def setLineJoint(newJoint: String) =
    new CMShape(
        commands,
        transform,
        colors,
        z,
        displayCondition,
        lineWidth,
        showSurcface,
        newJoint,
        closed,
        deltas,
        name
    )
  def setShowSurface(b: Boolean) =
    new CMShape(
        commands,
        transform,
        colors,
        z,
        displayCondition,
        lineWidth,
        b,
        lineJoint,
        closed,
        deltas,
        name
    )
  def setLineWidth(newWidth: Int) =
    new CMShape(
        commands,
        transform,
        colors,
        z,
        displayCondition,
        newWidth,
        showSurcface,
        lineJoint,
        closed,
        deltas,
        name
    )
def setClosed(isClosed :Boolean) = 
      new CMShape(
          commands,
          transform,
          colors,
          z,
          displayCondition,
          lineWidth,
          showSurcface,
          lineJoint,
          isClosed,
          deltas,
        name
        )
  
  def setDeltas( newDeltas :Seq[(String,Seq[(Int,CMShape)])]) =
     new CMShape(
        commands,
        transform,
        colors,
        z,
        displayCondition,
        lineWidth,
        showSurcface,
        lineJoint,
        closed,
        newDeltas,
        name
    )
  
  def setName(newName : String) = 
    new CMShape(
        commands,
        transform,
        colors,
        z,
        displayCondition,
        lineWidth,
        showSurcface,
        lineJoint,
        closed,
        deltas,
        newName
    )
  def changeId =
    new CMShape(
        commands,
        transform,
        colors,
        z,
        displayCondition,
        lineWidth,
        showSurcface,
        lineJoint,
        closed,
        deltas,
        name
    )
    
  
}
sealed trait DynamicColor {
  val value: String
  val alpha: Float

  def bindTo(bound: String) = new BoundColor(bound, alpha)
  def setConstantColor(c: String) = new ConstantColor(AllKnownColors.colorThis(c), alpha)
  def setAlpha(alpha: Float): DynamicColor

}
object DynamicColor {

  def apply(s: String, alpha: Float = 1f): DynamicColor = {
    val trimmed = s.trim()
    val m = Pattern.compile("^[CV]\\((1|0?\\.\\d+)\\)_").matcher(trimmed)
    if (m.find()) {
      val alphaString = m.group(1)
      val trueAlpha = alphaString.toFloat
      val value = trimmed.drop(4 + alphaString.length())
      val start = trimmed.charAt(0)
      if (start == 'C')
        ConstantColor(value, trueAlpha)
      else if (start == 'V')
        BoundColor(value, trueAlpha)
      else
        throw new IllegalArgumentException(s + " cannot be parsed to dynamic color")
    } else if (trimmed.startsWith("C_")) {
      val c = AllKnownColors.colorThis(trimmed.drop(2))
      ConstantColor(c, alpha)
    } else if (trimmed.startsWith("V_"))
      BoundColor(trimmed.drop(2), alpha)
    else if (trimmed.isEmpty() || trimmed == "None")
      ConstantColor("white", alpha)
    else
      throw new IllegalArgumentException(s + " cannot be parsed to dynamic color")
  }
}
sealed trait DrawCommand{
  def update(i : Int, v : Vec) : DrawCommand
  def pointAt(i : Int) : Vec
  def last : Vec
}
class MoveTo(val pos : Vec) extends DrawCommand {
  def update(i : Int, v : Vec) = {
    if(i !=0)
      throw new IllegalArgumentException("MoveTo have one and only one point")
    new MoveTo(v)
  }
  def pointAt(i : Int) = {
    if(i !=0)
      throw new IllegalArgumentException("MoveTo have one and only one point")
    pos
  }
  def last = pos
  
}
class CurveTo(val cp1 : Vec, val cp2 : Vec, val end :Vec) extends DrawCommand{
  def update(i : Int, v : Vec) = {
    if(i > 2 || i < 0)
      throw new NoSuchElementException("DrawCommandTo have 3 points. "+i+" is not ok")
    if(i == 0)
      new CurveTo(cp1,cp2,v)
    else if( i == 1)
      new CurveTo(v,cp2,end)
    else
      new CurveTo(cp1,v,end)
  }
  def pointAt(i : Int) = {
    if(i > 2 || i < 0)
      throw new NoSuchElementException("DrawCommandTo have 3 points. "+i+" is not ok")
    if(i == 0)
      cp1
    else if( i == 1)
      cp2
    else
      end
  }
  def last = end
}

object ConstantColor {
  def apply(v: String, a: Float) = new ConstantColor(v, a)
  def unapply(c: ConstantColor) = Some(c.value)

}
class ConstantColor(val value: String, val alpha: Float) extends DynamicColor {
  def setAlpha(a: Float) = new ConstantColor(value, a)
}
object BoundColor {
  def apply(v: String, a: Float) = new BoundColor(v, a)
  def unapply(b: BoundColor) = Some(b.value)
}
class BoundColor(val value: String, val alpha: Float) extends DynamicColor {
  def setAlpha(a: Float) = new BoundColor(value, a)

}