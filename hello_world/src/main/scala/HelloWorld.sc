object session {
//  println("Hello There!")

  def abs(x: Double) :Double = if (x < 0) -x else x

  def sqrt(x: Double): Double = {


    def isGoodEnough(guess: Double) = abs(guess * guess - x) / x < 1e-5

    def improve(guess: Double) = (guess + x / guess) / 2


    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    sqrtIter(1.0)
  }

  sqrt(10)
  sqrt(1e60)
  sqrt(1e-6)

  def test: Int = {
    val x = 0

    def f(y: Int) = y + 1

    val result = {
      val x = f(3)
      x * x
    } + x

    result
  }

  test

}