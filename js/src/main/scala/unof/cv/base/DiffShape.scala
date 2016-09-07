package unof.cv.base

import unof.cv.utils.Transforme
import unof.cv.base.charLib.DrawCommand

class DiffShape(
    val commands: Seq[DrawCommand],
    val transform: Transforme,
    val lineColor: (Float, Float, Float),
    val lineAlpha : Float,
    val surfaceColor: (Float, Float, Float),
    val surfaceAlpha : Float,
    val z: Float,
    val lineWidth: Float,
    val lineJoint: String,
    val closed: Boolean,
    val locationId : Int) {
} 