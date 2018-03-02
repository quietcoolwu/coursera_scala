
def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int =
    if (a > b) acc else loop(a + 1, f(a) + acc)

  loop(a, 0)
}

def product(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int =
    if (a > b) acc else loop(a + 1, f(a) * acc)

  loop(a, 1)
}


def mapReduceTailed(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
  //    TODO: combine operator
  def loop(a: Int, acc: Int): Int =
    if (a > b) acc else f(a) * loop(a + 1, acc)

  loop(a, zero)

}

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

}

def fact(n: Int): Int = mapReduce(x => x, (x, y) => x * y, 1)(1, n)


fact(5)
sum(x => x * x)(3, 5)
sum(fact)(3, 5)
product(x => x * x)(3, 5)
product(fact)(3, 5)

mapReduce(x => x * x, (x, y) => x * y, 1)(3, 5)
mapReduce(fact, (x, y) => x + y, 0)(3, 5)


