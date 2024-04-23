package ex2

object RobotTestUtils {
  extension (robot: Robot)
    def state: (Position, Direction) =
      (robot.position, robot.direction)
}
