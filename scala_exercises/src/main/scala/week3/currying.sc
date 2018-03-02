

def gcd(a: Int, b: Int): Int =
  if (b == 0) a else gcd(b, a % b)



def factorial(n: Int): Int = {
  def loop(acc: Int, n: Int): Int =
    if (n == 0) acc else loop(acc * n, n - 1)

  loop(1, n)
}

def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int =
    if (a > b) acc else loop(a + 1, f(a) + acc)

  loop(a, 0)
}


factorial(5)
sum(x => x * x)(3, 5)
sum(factorial)(3, 5)


gcd(100, 1500)
factorial(30)

