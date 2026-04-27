package ex2

import scala.util.Random

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot, var battery: Double, val amount: Double) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    if battery > amount then
      robot.act()
      battery -= amount
    else
      println("Recharge robot")

class RobotCanFail(val robot: Robot, val probability: Double) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    if Random.nextDouble() <= probability then
      robot.act()
    else
      println("Failed")

class RobotRepeated(val robot: Robot, val repeat: Int) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    for
      i <- 1 until repeat
    yield
      robot.act()


@main def testRobot(): Unit =
  val robot = SimpleRobot((0, 0), Direction.North)
  val robotLogging = LoggingRobot(robot)
  robotLogging.act() // robot at (0, 1) facing North
  robotLogging.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robotLogging.act() // robot at (1, 1) facing East
  robotLogging.act() // robot at (2, 1) facing East
  val battery = 100
  val decrement = 30
  val robotWithBattery = RobotWithBattery(robotLogging, battery, decrement)
  for
    i <- 1 to (battery / decrement) + 1
  yield
    robotWithBattery.act()

  val probability = 0.50
  val robotCanFail = RobotCanFail(robotLogging, probability)
  for
    i <- 1 to 10
  yield
    robotCanFail.act()

  val repetitions = 5
  val robotRepeated = RobotRepeated(robotLogging, repetitions)
  robotRepeated.act()