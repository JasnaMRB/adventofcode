package year2021

import scala.annotation.tailrec

object Day3 {
  private val numIndices: Int = 12
  def getReport(filePath: String): List[String] = {
    val bufferedSource = io.Source.fromFile(filePath)
    try bufferedSource.getLines().toList
    finally bufferedSource.close()
  }

  private def binaryToDecimal(str: String): Double = {
    str.foldLeft(0.0, numIndices) {
      case ((total, power), char) =>
        val nextTotal = if (char == '1') scala.math.pow(2, power) else 0.0
        (total + nextTotal, power - 1)
    }._1
  }

  def getPowerConsumption(filePath: String): Double = {
    val report = getReport(filePath)
    val total = report.size
    val counts = report.foldLeft(List.iterate(0, numIndices)(_ => 0)) {
      case (acc, line) =>
        line.zipWithIndex.foldLeft(acc) {
          case (a, (char, index)) => a.updated(index, a(index) + char.asDigit)
        }
    }.map{ ones =>
      val zeroes = total - ones
      val most = if (ones > zeroes) 1 else 0
      val least = if (most == 1) 0 else 1
      (most, least)
    }
    def getRate(bits: List[Int]): Double = binaryToDecimal(bits.mkString(""))
    val gammaRate = getRate(counts.map(_._1))
    val epsilonRate = getRate(counts.map(_._2))
    gammaRate * epsilonRate
  }

  def getLifeSupportRating(filePath: String): Double = {
    val report = getReport(filePath)
    def getRating(f: (List[String], List[String]) => List[String]): Double = {
      @tailrec
      def traverse(acc: List[String], index: Int): String = {
        if (acc.size == 1) acc.head else {
          val (ones, zeroes) = acc.partition(_(index) == '1')
          val next = f(ones, zeroes)
          traverse(next, index + 1)
        }
      }
      binaryToDecimal(traverse(report, index = 0))
    }
    val oxygenGeneratorRating = getRating((a, b) => if (a.size >= b.size) a else b)
    val co2ScrubberRating = getRating((a, b) => if (b.size <= a.size) b else a)
    oxygenGeneratorRating * co2ScrubberRating
  }
}

/**
 * Day3.getPowerConsumption(filePath) // part 1
 * Day3.getLifeSupportRating(filePath // part 2
 */
