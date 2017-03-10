package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)

    val standingBlock = Block(Pos(1, 3), Pos(1, 3))
    val lyingBlock = Block(Pos(1, 3), Pos(1, 4))
    val partiallyOnBlock = Block(Pos(1, 5), Pos(1, 6))
    val offBlock = Block(Pos(1, 6), Pos(1, 7))
    val standingDestBlock = Block(Pos(4, 7), Pos(4, 7))
    val lyingDestBlock = Block(Pos(4, 7), Pos(4, 8))
  }

  trait LevelWithNoSolution extends SolutionChecker {
    /* terrain for level with no solution*/

    val level =
      """----------
        |-ST-------
        |----------
        |----------
        |----------
        |----------""".stripMargin

    val optsolution = List()
  }


	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("isStanding: when block stands") {
    new Level1 {
      assert(standingBlock.isStanding)
    }
  }

  test("isStanding: when block lies down") {
    new Level1 {
      assert(!lyingBlock.isStanding)
    }
  }

  test("isLegal: when block is entirely on terrain") {
    new Level1 {
      assert(standingBlock.isLegal)
      assert(lyingBlock.isLegal)
    }
  }

  test("isLegal: when block is partially on terrain") {
    new Level1 {
      assert(!partiallyOnBlock.isLegal)
    }
  }

  test("isLegal: when block is off terrain") {
    new Level1 {
      assert(!offBlock.isLegal)
    }
  }

  test("done: when block is standing on destination") {
    new Level1 {
      assert(done(standingDestBlock))
    }
  }

  test("done: when block is lying on destination") {
    new Level1 {
      assert(!done(lyingDestBlock))
    }
  }

  test("startBlock") {
    new Level1 {
      assert(startBlock.isStanding)
      assert(startBlock.b1.row == 1)
      assert(startBlock.b1.col == 1)
      assert(startBlock.b2.row == 1)
      assert(startBlock.b2.col == 1)
    }
  }

  test("neighborsWithHistory") {
    new Level1 {
      val neighborsWithHistorySet = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)).toSet
      val expected = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      )
      assert(neighborsWithHistorySet== expected)
    }
  }

  test("newNeighborsOnly") {
    new Level1 {
      val newNeighborsOnlySet = newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).toStream,
        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      ).toSet
      val expected = Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      )
      assert(newNeighborsOnlySet == expected)
    }
  }

	test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

  test("optimal solution for level with no solution") {
    new LevelWithNoSolution {
      assert(solution == List())
    }
  }
}
