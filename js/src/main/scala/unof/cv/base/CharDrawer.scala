package unof.cv.base

import Algebra.DDVector

object DrawChar extends Drawer {
  def apply(char: Character, context: DrawingContext) {
    val ctx = context.ctx
    ctx.setTransform(1, 0, 0, 1, 0, 0)
    ctx.clearRect(0, 0, context.dimensions.x, context.dimensions.y)
    char.parts
      .foreach { l =>
        
        try {
          l match {
            case part: CharacterImagePart =>
              drawImage(part.image, part.color, char.transforms ++ part.transforms , context)
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
                context)
          }
        } catch {
          case t: Throwable => // fuck this
        }
      }
  }
}