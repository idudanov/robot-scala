package robo

import org.scalatest.{FunSuite, Matchers}

import cats.data.State

class RobotTest extends FunSuite with Matchers {

  test("(place) should give false if outside a tabletop") {
    val result = (for {
      s <- Robot.place(7, 7, North).get

    } yield s).runA(Robot()).value

    assert(result.placed === false)
  }

  test("(place) should give true if inside a tabletop") {
    val result = (for {
      s <- Robot.place(0, 0, North).get

    } yield s).runA(Robot()).value

    assert(result.placed === true)
  }

  test("(left) turn should change from North to West if a robot is placed") {
    val result = (for {
      _ <- Robot.place(0, 0, North)
      s <- Robot.left.get

    } yield s).runA(Robot()).value

    assert(result.d === West)
  }

  test("(left) turn should NOT change from North to West if a robot is NOT placed") {
    val result = (for {
      _ <- Robot.place(7, 7, North)
      s <- Robot.left.get

    } yield s).runA(Robot()).value

    assert(result.d === North)
  }

  test("(right) turn should change from North to East if a robot is placed") {
    val result = (for {
      _ <- Robot.place(0, 0, North)
      s <- Robot.right.get

    } yield s).runA(Robot()).value

    assert(result.d === East)
  }

  test("(right) turn should NOT change from North to East if a robot is NOT placed") {
    val result = (for {
      _ <- Robot.place(7, 7, North)
      s <- Robot.right.get

    } yield s).runA(Robot()).value

    assert(result.d === North)
  }

  test("(move) should be ignored if a robot is NOT placed") {
    val result = (for {
      _ <- Robot.place(7, 7, North)
      s <- Robot.move.get

    } yield s).runA(Robot()).value

    assert(result.y === 0)
  }

  test("(move) to East should increase X") {
    val result = (for {
      _ <- Robot.place(0, 0, East)
      s <- Robot.move.get

    } yield s).runA(Robot()).value

    assert(result.x === 1)
  }

  test("(move) to West should decrease X") {
    val result = (for {
      _ <- Robot.place(1, 0, West)
      s <- Robot.move.get

    } yield s).runA(Robot()).value

    assert(result.x === 0)
  }

  test("(move) to East should NOT result in falling of a tabletop if X > 5") {
    val result = (for {
      _ <- Robot.place(5, 0, East)
      s <- Robot.move.get

    } yield s).runA(Robot()).value

    assert(result.x === 5)
  }

  test("(move) to West should NOT result in falling of a tabletop if X < 0") {
    val result = (for {
      _ <- Robot.place(0, 0, West)
      s <- Robot.move.get

    } yield s).runA(Robot()).value

    assert(result.x === 0)
  }

  test("(move) to North should increase Y") {
    val result = (for {
      _ <- Robot.place(0, 0, North)
      s <- Robot.move.get

    } yield s).runA(Robot()).value

    assert(result.y === 1)
  }

  test("(move) to South should decrease Y") {
    val result = (for {
      _ <- Robot.place(0, 1, South)
      s <- Robot.move.get

    } yield s).runA(Robot()).value

    assert(result.y === 0)
  }

  test("(move) to North should NOT result in falling of a tabletop if Y > 5") {
    val result = (for {
      _ <- Robot.place(0, 5, North)
      s <- Robot.move.get

    } yield s).runA(Robot()).value

    assert(result.y === 5)
  }

  test("(move) to South should NOT result in falling of a tabletop if Y < 0") {
    val result = (for {
      _ <- Robot.place(0, 0, South)
      s <- Robot.move.get

    } yield s).runA(Robot()).value

    assert(result.y === 0)
  }

  test("(move) number six should be ignored, but the next command should be accepted") {
    val result = (for {
      _ <- Robot.place(0, 0, North)
      _ <- Robot.move
      _ <- Robot.move
      _ <- Robot.move
      _ <- Robot.move
      _ <- Robot.move
      _ <- Robot.move
      s <- Robot.right.get

    } yield s).runA(Robot()).value

    assert(result === Robot(true, 0, 5, East))
  }

  test("PLACE 1,2,EAST should be decoded correctly") {
    val p = RobotApp.decPlace("PLACE 1,2,EAST")
    assert(p._1 === 1)
    assert(p._2 === 2)
    assert(p._3 === East)
  }

  test("PLACE 0,0,NORTH should be decoded correctly") {
    val p = RobotApp.decPlace("PLACE 0,0,NORTH")
    assert(p._1 === 0)
    assert(p._2 === 0)
    assert(p._3 === North)
  }

  test("Robot should accept only valid commands") {
    val valid = List("MOVE", "LEFT", "RIGHT", "REPORT", "PLACE 0,0,NORTH", "PLACE 1,1,EAST")
    val invalid = List("FLY", "JUMP", "RUN", "PLACE x,1,EAST", "PLACE 1,y,EAST", "PLACE 0,0,5555")
    assert(valid.size === (valid ++ invalid.filter(s => RobotApp.filterCommands(s))).size)
  }
}
