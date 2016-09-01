package unof.cv.base

object Algebra {

  implicit def diVec(dd: (Double, Int)): DDVector = new DDVector(dd._1, dd._2)
  implicit def idVec(dd: (Int, Double)): DDVector = new DDVector(dd._1, dd._2)
  implicit def iiVec(dd: (Int, Int)): DDVector = new DDVector(dd._1, dd._2)
  implicit def ffVec(dd: (Float, Float)): DDVector = new DDVector(dd._1, dd._2)
  implicit def jsVec(jsv : JSVec) : DDVector = new DDVector(jsv.x.doubleValue(),jsv.y.doubleValue())
  implicit def jsVecToDD(jsv : JSVec) : Vec = (jsv.x.doubleValue(),jsv.y.doubleValue())
  type Vec = (Double, Double)
  implicit class DDVector(val t: (Double, Double)) extends AnyVal {
    def x = t._1.toInt
    def y = t._2.toInt
    def sqrNorm = t._1 * t._1 + t._2 * t._2
    def norm = math.sqrt(sqrNorm)
    private def genZipOp(f: (Double, Double) => Double)(v: Vec): Vec = (f(t._1, v._1), f(t._2, v._2))
    def + = genZipOp(_ + _)_
    def - = genZipOp(_ - _)_
    def * = genZipOp(_ * _)_
    def *(d: Double): Vec = (t._1 * d, t._2 * d)
    def /(d: Double): Vec = (t._1 / d, t._2 / d)
    def min = genZipOp(_ min _)_
    def max = genZipOp(_ max _)_
    def / = genZipOp(_ / _)_
    def unary_- : Vec = (-t._1, -t._2)
    def abs = (t._1.abs,t._2.abs)
    
    private def genCompOp(f: (Double, Double) => Boolean)(v: Vec): Boolean = f(t._1, v.t._1) && f(t._2, v.t._2)
    /**
     * @return true if the relation is true for the two t._1 and true for the two t._2
     */
    def < = genCompOp(_ < _)_
    /**
     * @return true if the relation is true for the two t._1 and true for the two t._2
     */
    def <= = genCompOp(_ <= _)_
    /**
     * @return true if the relation is true for the two t._1 and true for the two t._2
     */
    def > = genCompOp(_ > _)_
    /**
     * @return true if the relation is true for the two t._1 and true for the two t._2
     */
    def >= = genCompOp(_ >= _)_
    def dot(v: Vec) = t._1 * v._1 + t._2 * v._2
    def direction = this / this.norm

    def piRotate = (-t._2, t._1)
    def minusPiRotate = (t._2, -t._1)

    def <-> (v :Vec) = {
      
      math.sqrt(this <<->> v)
    }
    def <<->> (v : Vec) = {
      val dif = this - v
      dif dot dif
    }
    
    implicit def toNN: (Number, Number) = (t._1, t._2)
  }
}
