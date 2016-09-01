package unof.cv.base

import org.scalajs.dom.raw.{ HTMLImageElement => Image }

import unof.cv.base.charmaker.DrawCommand

class Character (
  val parts : Seq[CharacterPart],
  val transforms : Seq[Transforme]
)
sealed trait CharacterPart{
    val imageId : Int
    val transforms:Seq[Transforme]
    //def setTransfromes(newTransforms:Seq[Transforme]):  this.type
}
class CharacterImagePart(
    val image : Image,
    val color : String,
    val transforms:Seq[Transforme],
    val imageId : Int
  )extends CharacterPart{
  
}
  
class CharacterShapePart(
  val lineColor : String,
  val lineAlpha : Float,
  val surfaceColor : String,
  val surfaceAlpha : Float,
  val lineWidth : Int,
  val lineJoin : String,
  val showSurcface : Boolean,
  val controlPoints : Seq[DrawCommand],
  val transforms:Seq[Transforme],
  val imageId : Int,
  val isClosed : Boolean
)extends CharacterPart {
  def setControlPoints(newControlPoints : Seq[DrawCommand]):CharacterShapePart = {
    new CharacterShapePart(
        lineColor,
        lineAlpha,
        surfaceColor,
        surfaceAlpha,
        lineWidth,
        lineJoin,
        showSurcface,
        newControlPoints,
        transforms,
        imageId,
        isClosed
    )
        
  }
  /*def setTransfromes(newTransforms:Seq[Transforme]):CharacterShapePart = {
    new CharacterShapePart(
        lineColor,
        lineAlpha,
        surfaceColor,
        surfaceAlpha,
        lineWidth,
        lineJoin,
        showSurcface,
        controlPoints,
        newTransforms,
        imageId,
        isClosed
    )
        
  }*/
}