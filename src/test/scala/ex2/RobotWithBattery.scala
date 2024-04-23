package ex2

import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterEach

class RobotWithBatteryTest extends AnyFunSuite with Matchers with BeforeAndAfterEach:
    private val batteryCapacity = 100
    private val batteryActionCost = 50
    private var robot: RobotWithBattery = _
    
    override protected def beforeEach(): Unit = 
        robot = new RobotWithBattery(robot = new SimpleRobot((0, 0), Direction.North), _batteryCapacity = batteryCapacity, batteryActionCost = batteryActionCost)

    test("Should start with maximum battery"):
        robot.batteryCapacity shouldBe batteryCapacity
    
    test("Act should cost battery"):
        robot.act()
        robot.batteryCapacity shouldBe batteryCapacity - batteryActionCost
    
    test("Act should do nothing if battery is empty"):
        robot.act()
        robot.act()
        val state = (robot.position, robot.direction)
        robot.act()
        (robot.position, robot.direction) shouldBe state
