enum Direction:
  case UP, DOWN, LEFT, RIGHT

object Direction:
  def get(char: Char): Direction = char match
    case 'U' | '3' => Direction.UP
    case 'D' | '1' => Direction.DOWN
    case 'L' | '2' => Direction.LEFT
    case 'R' | '0' => Direction.RIGHT