package year2021

object Day2 {
  sealed trait Direction
  object Direction {
    case object Forward extends Direction
    case object Down extends Direction
    case object Up extends Direction

    def toDirection(str: String): Direction = {
      str match {
        case "forward" => Forward
        case "down" => Down
        case "up" => Up
        case _ => throw new Exception(s"Cannot parse direction: $str")
      }
    }
  }

  case class Command(dir: Direction, pos: Int)

  def getCommands(filePath: String): List[Command] = {
    val bufferedSource = io.Source.fromFile(filePath)
    try bufferedSource.getLines().map { str =>
      val strArray = str.split(" ")
      Command(Direction.toDirection(strArray(0)), strArray(1).toInt)
    }.toList
    finally bufferedSource.close()
  }

  def getPosition(commands: List[Command]): Int = {
    val (h, d) = commands.foldLeft((0, 0)) {
      case ((horizontalPosition, depth), command) =>
        command.dir match {
          case Direction.Forward => (horizontalPosition + command.pos, depth)
          case Direction.Up => (horizontalPosition, depth - command.pos)
          case Direction.Down => (horizontalPosition, depth + command.pos)
        }
    }
    h * d
  }
}

/**
 * val commands = Day2.getCommands
 * Day2.getPosition(commands)
 **/
