
  val x = new Rational(1,3)
  x.numer
  x.denom

  val y = new Rational(5,7)
  x + y

  -x

  val z = new Rational(3,2)

  x - y - z
  y + y
  x < y
  x max y
  new Rational(2)

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  def numer = x / g  // divide by g
  def denom = y / g // divide by g

  def < (that: Rational) = numer * that.denom < denom * that.numer

  def max(that: Rational) = if (this < that) that else this

  def + (that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  override def toString = numer + "/" + denom

  def unary_- : Rational = new Rational(-numer, denom) //need a space tween - and :

  def - (that: Rational) = this + -that

}
