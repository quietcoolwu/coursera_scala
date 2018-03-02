import math.abs


val tolerance = 0.0001

def isCloseEnough(x: Double, y: Double) =
  abs((x - y) / x) / x < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    println("guessBefore =" + guess)
    val next = f(guess)
    println("guessAfter, next = " + guess + next)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }

  iterate(firstGuess)
}


def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2


def sqrtSimple(x: Double) = fixedPoint(y => (y + x / y) / 2)(10.0)

def sqrtDamp(x: Double) = fixedPoint(averageDamp(y => x / y))(10.0)


fixedPoint(x => 1 + x / 2)(3.0)

// not converge if using y=>x/y directly
sqrtSimple(2.0)

sqrtDamp(2.0)
