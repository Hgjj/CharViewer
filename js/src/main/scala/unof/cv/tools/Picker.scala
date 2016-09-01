package unof.cv.tools

import unof.cv.base.Algebra.DDVector
import unof.cv.base.Algebra.Vec
import unof.cv.base.Character
import unof.cv.base.CharacterImagePart
import unof.cv.base.CharacterPart
import unof.cv.base.CharacterShapePart
import unof.cv.base.Drawer
import unof.cv.base.DrawingContext

object Picker extends Drawer {
  def pick(at: Vec, char: Character, context: DrawingContext) = {
    val myContext = new DrawingContext("", context.dimensions)

    def isPicked(part: CharacterPart): Boolean = {
      myContext.ctx.clearRect(0, 0, myContext.dimensions.x, myContext.dimensions.y)

      part match {
        case img: CharacterImagePart =>
          !(img.image.src == "") && {
            drawImage(img.image, "white", char.transforms ++ img.transforms, myContext)
            myContext.ctx.getImageData(at.x, at.y, 1, 1).data(3) > 0
          }
        case shape: CharacterShapePart =>
          drawShape(
            shape.controlPoints,
            shape.surfaceColor,
            shape.surfaceAlpha,
            shape.showSurcface,
            shape.lineColor,
            shape.lineAlpha,
            shape.lineWidth,
            shape.lineJoin,
            char.transforms ++ shape.transforms,
            shape.isClosed,
            myContext)
          myContext.ctx.getImageData(at.x, at.y, 1, 1).data(3) > 0
      }

    }
    try {
      char.parts.lastIndexWhere { isPicked }
      
    } catch {
      case _: Throwable => -1
    }

    /*if(reversIndex <0)
      reversIndex
    else
      char.parts.size -1 - reversIndex*/
  }
}