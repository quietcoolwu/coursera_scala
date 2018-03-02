class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  val numer = x / gcd(x, y)

  val denom = y / gcd(x, y)

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def this(x: Int) = this(x, 1)


  def <(that: Rational) = {
    (this.denom * that.denom) * (this.numer * that.denom - this.denom * that.numer) < 0
  }

  def max(that: Rational) =
    if (this < that) that else this

  def min(that: Rational) =
    if (!(this < that)) that else this

  //  prefix operator
  def unary_- : Rational = new Rational(-numer, denom)


  def +(r: Rational) =
    new Rational(numer * r.denom + r.numer * denom,
      denom * r.denom)

  def -(r: Rational) = this + -r


  def *(r: Rational) =
    new Rational(numer * r.numer,
      denom * r.denom)

  def /(r: Rational) = this * new Rational(r.denom, r.numer)

  override def toString = numer + "/" + denom
}


val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
// nonzero exception
//val strange = new Rational(3, 0)

x < -x

-x < x

x - y - z

x + y + z

x * y + y * z

x / y
y / x
new Rational(1) / x


x < y
x max y
y max z
x min z
