package ex2

import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterEach
import RobotTestUtils.*

class RobotRepeatedTest extends AnyFunSuite with Matchers with BeforeAndAfterEach:
    private val repetitions = 2
    private var robot: RobotRepeated = _

    private def newDefaultRobot(repetitions: Int) =
        new RobotRepeated(robot = new SimpleRobot((0, 0), Direction.North), repetitions = repetitions)
    
    override protected def beforeEach(): Unit = 
        robot = newDefaultRobot(repetitions)
    
    test("repetitions argument is validated"):
        a [IllegalArgumentException] should be thrownBy newDefaultRobot(0)
        a [IllegalArgumentException] should be thrownBy newDefaultRobot(-1)
    
    test("act repeats correctly"):
        robot.act()
        robot.state shouldBe ((0, 2), robot.state._2)
