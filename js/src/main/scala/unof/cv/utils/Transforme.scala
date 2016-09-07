package unof.cv.utils
import scala.scalajs.js
import scala.scalajs.js.Any.jsArrayOps
import Algebra._
import unof.cv.utils.Algebra

object Transforme {
  def apply(
    sx: Double,
    sy: Double,
    rotation: Double,
    dx: Double,
    dy: Double) = {
    val cosR = math.cos(rotation / 360 * 2 * math.Pi)
    val sinR = math.sin(rotation / 360 * 2 * math.Pi)

    def m11 = sx * cosR
    def m12 = -sx * sinR
    def m21 = sy * sinR
    def m22 = sy * cosR
    new Transforme(sx, sy, rotation, dx, dy, m11, m12, m21, m22)
  }
  def apply(t: js.Array[Number]): Transforme = {
    if (t.isEmpty) {
      Transforme()
    } else {
      val seq = t.map(_.floatValue()).toSeq
      if (seq.size != 5) {
        throw new IllegalArgumentException(" The number of value for a transforme must be six.\nThus " + t + " is not ok")
      }
      Transforme(seq(0), seq(1), seq(2), seq(3), seq(4))
    }

  }
  def apply(): Transforme = Transforme(1, 1, 0, 0, 0)

}
sealed class Transforme(
    val sx: Double,
    val sy: Double,
    val rotation: Double,
    val dx: Double,
    val dy: Double,
    val m11: Double,
    val m12: Double,
    val m21: Double,
    val m22: Double) {

  def *(v: Vec) = {
    val x = m11 * v._1 + m21 * v._2 + dx
    val y = m12 * v._1 + m22 * v._2 + dy
    (x, y)
  }
  def a = m11
  def b = m12
  def c = m21
  def d = m22
  def e = dx
  def f = dy
  def *(t: Transforme) = {

    val n11 = m11 * t.a + m21 * t.b
    val n12 = m12 * t.a + m22 * t.b
    val n21 = m11 * t.c + m21 * t.d
    val n22 = m12 * t.c + m22 * t.d
    val n31 = m11 * t.e + m21 * t.f + dx
    val n32 = m12 * t.e + m22 * t.f + dy

    new Transforme(Double.NaN, Double.NaN, Double.NaN, n31, n32, n11, n12, n21, n22)

  }
  
  def invert = {
    val m = (a * d - b * c)

    val n11 = d / m
    val n12 = -b / m
    val n21 = -c / m
    val n22 = a / m
    val n31 = (c * dy - d * dx) / m
    val n32 = (b * dx - a * dy) / m

    new Transforme(Double.NaN, Double.NaN, Double.NaN, n31, n32, n11, n12, n21, n22)
    
  }
  def isInteroplatable = !m11.isNaN()
  override def toString = Seq(sx, sy, rotation, dx, dy) mkString ("[ ", ", ", " ]")
  def trueString = Seq(m11, m21, dx, "\n", m12, m22, dy) mkString ("[ ", ", ", " ]")
}