package ex2

import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterEach

class RobotCanFailTest extends AnyFunSuite with Matchers with BeforeAndAfterEach:
    private val successRate = 0.5
    private var robot: RobotCanFail = _

    extension (robot: Robot) def state: (Position, Direction) =
        (robot.position, robot.direction)

    private def newDefaultRobot(successRate: Double) =
        new RobotCanFail(robot = new SimpleRobot((0, 0), Direction.North), successRate = successRate)
    
    override protected def beforeEach(): Unit = 
        robot = newDefaultRobot(successRate)
    
    test("successRate argument is validated"):
        a [IllegalArgumentException] should be thrownBy newDefaultRobot(1.1)
        a [IllegalArgumentException] should be thrownBy newDefaultRobot(-1)
    
    test("act failure"):
        val robot = newDefaultRobot(0)
        val state = robot.state
        robot.act()
        robot.state shouldBe state

    test("act success"):
        val robot = newDefaultRobot(1)
        val state = robot.state
        robot.act()
        robot.state should not be state
