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
      val Array(dir, pos) = str.split(" ")
      Command(Direction.toDirection(dir), pos.toInt)
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

  def getPositionWithAim(commands: List[Command]): Long = {
    val (h, d, _) = commands.foldLeft((0, 0, 0)) {
      case ((horizontalPosition, depth, aim), command) =>
        command.dir match {
          case Direction.Forward => (horizontalPosition + command.pos, depth + (command.pos * aim), aim)
          case Direction.Up => (horizontalPosition, depth, aim - command.pos)
          case Direction.Down => (horizontalPosition, depth, aim + command.pos)
        }
    }
    h * d
  }
}


/**
 * val commands = Day2.getCommands
 * Day2.getPosition(commands)
 **/
