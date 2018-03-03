class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  val numer = x / gcd(x, y)

  val denom = y / gcd(x, y)

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def this(x: Int) = this(x, 1)


  def <(that: Rational) =
    (denom * that.denom) * (numer * that.denom - denom * that.numer) < 0


  def min(that: Rational) =
    if (this < that) this else that

  //  prefix operator
  def unary_- : Rational = new Rational(-numer, denom)


  def +(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom,
      denom * that.denom)

  def -(that: Rational) = this + (-that)


  def *(that: Rational) =
    new Rational(numer * that.numer,
      denom * that.denom)

  def /(that: Rational) = this * new Rational(that.denom, that.numer)

  override def toString = numer + "/" + denom
}


val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
// nonzero exception
//val strange = new Rational(3, 0)

x < -x

-x < x

-x - y - z

x + y + z

x * y + y * z

x / y
y / x
new Rational(1) / x


x < y
x min z
