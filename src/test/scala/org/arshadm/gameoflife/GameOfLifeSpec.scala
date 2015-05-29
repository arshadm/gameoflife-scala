package org.arshadm.gameoflife

import org.scalatest._

class GameOfLifeSpec extends FlatSpec with Matchers {

  "Setting x position cell status out of bounds" should "throw assertion error" in {
    intercept[AssertionError] {
      val gameOfLife = (new GameOfLife(width = 10, height = 10)).setCellStatus(10, 2, true)
    }
  }

  "Setting y position cell status out of bounds" should "throw assertion error" in {
    intercept[AssertionError] {
      val gameOfLife = (new GameOfLife(width = 10, height = 10)).setCellStatus(2, 10, true)
    }
  }

  "Setting cell status to live" should "result in a live cell" in {
    val gameOfLife = new GameOfLife
    gameOfLife.getCellStatus(5, 5) should be (false)

    val updatedGameOfLife = gameOfLife.setCellStatus(5, 5, true)

    updatedGameOfLife.getCellStatus(5, 5) should be (true)
  }

  "Setting cell status to dead" should "result in a dead cell" in {
    val gameOfLife = (new GameOfLife).setCellStatus(3, 3, true)
    gameOfLife.getCellStatus(3, 3) should be (true)

    val updatedGameOfLife = gameOfLife.setCellStatus(3, 3, false)

    updatedGameOfLife.getCellStatus(3, 3) should be (false)
  }

  "Live cell with less than 2 live neighbours" should "die" in {
    val gameOfLife = (new GameOfLife)
      .setCellStatus(3, 3, true)
      .setCellStatus(10, 10, true)
      .setCellStatus(11, 11, true)

    val updatedGameOfLife = gameOfLife.next()

    updatedGameOfLife.getCellStatus(3, 3) should be (false)
    updatedGameOfLife.getCellStatus(10, 10) should be (false)
    updatedGameOfLife.getCellStatus(11, 11) should be (false)
  }

  "Live cell with 2 or 3 live neighours" should "stay alive" in {
    val gameOfLife = (new GameOfLife)
      .setCellStatus(3, 3, true)
      .setCellStatus(2, 2, true)
      .setCellStatus(3, 2, true)

    val updatedGameOfLife = gameOfLife.next()

    updatedGameOfLife.getCellStatus(3, 3) should be (true)
  }

  "Live cell with 3 or more live neighbours" should "die" in {
    val gameOfLife = (new GameOfLife)
      .setCellStatus(3, 3, true)
      .setCellStatus(2, 2, true)
      .setCellStatus(2, 3, true)
      .setCellStatus(2, 4, true)
      .setCellStatus(3, 2, true)

    val updatedGameOfLife = gameOfLife.next()

    updatedGameOfLife.getCellStatus(3, 3) should be (false)
  }

  "Dead cell with exactly 3 live neighbours" should "become live" in {
    val gameOfLife = (new GameOfLife)
      .setCellStatus(2, 2, true)
      .setCellStatus(2, 3, true)
      .setCellStatus(2, 4, true)

    val updatedGameOfLife = gameOfLife.next()

    updatedGameOfLife.getCellStatus(3, 3) should be (true)
  }

  "Cell on edge of world" should "update correctly" in {
    val gameOfLife = (new GameOfLife(width = 10, height = 10))
      .setCellStatus(0, 0, true)
      .setCellStatus(9, 9, true)
      .setCellStatus(9, 0, true)

    val updatedGameOfLife = gameOfLife.next()

    updatedGameOfLife.getCellStatus(0, 0) should be (true)
  }
}
