package week4

class ChecksumAccumulator {
  private var sum = 0

  def add(b: Byte): Unit = sum += b

  def checkSum(): Int = ~(sum & 0xFF) + 1
}


import scala.collection.mutable

object ChecksumAccumulator {
  private val cache = mutable.Map.empty[String, Int]

  def calc(s: String): Int =
    if (cache.contains(s)) cache(s)
    else {
      val acc = new ChecksumAccumulator
      for (c <- s) acc.add(c.toByte)
      val cs = acc.checkSum()
      cache += (s -> cs)
      cs
    }
}


