package robo

import cats.data.State
import cats.implicits._


sealed trait Direction

case object North extends Direction
case object West extends Direction
case object South extends Direction
case object East extends Direction

case class Robot(placed:Boolean, x:Int, y:Int, d:Direction)

object Robot {

  type RobotState[A] = State[Robot, A]
  val maxX, maxY = 5
  val minX, minY = 0

  val placed = true
  val notPlaced = false

  /**
    * Checks a tabletop boundaries
    * Places a robot on a tabletop by returning a RobotState
    * Initializes a new state inside monad or overwrites a current one
    * @param x X coordinate
    * @param y Y coordinate
    * @param d "OR" type algebraic data type that represents direction North | West | South | East
    * @return RobotState[Unit]
    */
  def place(x:Int, y:Int, d:Direction ): RobotState[Unit] = {
    if (x < minX || x > maxX || y < minY || y > maxY)
      State.set[Robot](Robot())
    else
      State.set[Robot](Robot(placed, x, y, d))
  }

  /**
    * Handles movement by applying pattern matching then cloning a current State
    * Increases or decreases X or Y depending on a current State
    * @return RobotState[Unit]
    */
  def move:RobotState[Unit] = State.modify[Robot]{
    case r@Robot(false, _, _, _) => r
    case r@Robot(true, _, y, North) => if (y >= maxY) r else r.copy(y = y + 1)
    case r@Robot(true,x, _, West) => if (x <= minX) r else r.copy(x = x - 1)
    case r@Robot(true,_, y, South) => if (y <= minY) r else r.copy(y = y - 1)
    case r@Robot(true,x, _, East) => if (x >= maxX) r else r.copy(x = x + 1)
  }

  /**
    * Handles right rotation by applying pattern matching then cloning a current State
    * Modifies Direction depending on a current State
    * @return RobotState[Unit]
    */
  def right:RobotState[Unit] = State.modify[Robot]{
    case r@Robot(false, _, _, _) => r
    case r@Robot(true, _, _, North) =>  r.copy(d = East)
    case r@Robot(true,_, _, West) => r.copy(d = North)
    case r@Robot(true,_, _, South) => r.copy(d = West)
    case r@Robot(true,_, _, East) =>  r.copy(d = South)
  }

  /**
  * Handles left rotation by applying pattern matching then cloning a current State
  * Modifies Direction depending on a current State
  * @return RobotState[Unit]
  */
  def left:RobotState[Unit] = State.modify[Robot]{
    case r@Robot(false, _, _, _) => r
    case r@Robot(true,_, _, North) =>  r.copy(d = West)
    case r@Robot(true,_, _, West) => r.copy(d = South)
    case r@Robot(true,_, _, South) => r.copy(d = East)
    case r@Robot(true,_, _, East) =>  r.copy(d = North)
  }

  /**
    * Shows a current State of a robot
    * @return RobotState[String]
    */
  def report:RobotState[String] = State.get[Robot].map {
    case r@Robot(false, _, _, _) => "Robot is NOT placed"
    case Robot(true, x, y, d) =>   s"Robot's position X = $x, Y = $y, Direction = $d"
  }

  /**
    * Robot identity function
    * @return Robot
    */
  def apply():Robot = Robot(notPlaced, minX, minY, North)
}

/*
 * This is an example of robot movements
 * I made a decision to stick to a library style implementation.
 * Therefore, this is how a client code should look like this:
 */
object RobotApp {

  def main(args: Array[String]): Unit = {

    val result = (for {
      _ <- Robot.place(0,0, North)
      _ <- Robot.right
      _ <- Robot.move
      _ <- Robot.move
      p <- Robot.report
    } yield p).runA(Robot()).value

    println(result)
  }
}