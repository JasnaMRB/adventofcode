package year2021

object Day1 {

  def getDepths(filePath: String): List[Int] = {
    val bufferedSource = io.Source.fromFile(filePath)
    try bufferedSource.getLines().map(_.toInt).toList
    finally bufferedSource.close()
  }

  def getNumLargerMeasurements(depths: List[Int]): Int = {
    depths.tail.foldLeft((0,depths.head)){
      case ((acc, prevDepth), depth) =>
        val nextAcc = if (depth > prevDepth) acc + 1 else acc
        (nextAcc, depth)
    }._1
  }

  def getSlidingNumLargerMeasurements(depths: List[Int]): Int = {
    val slide = depths.sliding(3).map(_.sum).toList
    getNumLargerMeasurements(slide)
  }
}
/**
 * val depths = Day1.getDepths
 * Day1.getNumLargerMeasurements(depths)
 * Day1.getSlidingNumLargerMeasurements(depths)
 **/