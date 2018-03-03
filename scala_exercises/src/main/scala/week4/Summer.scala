package week4

import ChecksumAccumulator.calc

object Summer {
  def main(args: Array[String]): Unit = {
    for (arg <- args) println(arg + ": " + calc(arg))
  }
}

object PrintSeasons extends App {
  for (season <- List("Spring", "Summer", "Fall", "winter")) println(season + ": " + calc(season))
}
