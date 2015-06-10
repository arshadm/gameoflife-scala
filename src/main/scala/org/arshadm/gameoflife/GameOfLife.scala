package org.arshadm.gameoflife

import scala.collection.immutable.HashSet
import scala.collection.mutable.ListBuffer

object GameOfLife {
  type Cell = Product2[Int, Int]
}

class GameOfLife (val world: Set[GameOfLife.Cell] = Set.empty, val width: Int = 100, val height: Int = 100) {
  def getCellStatus(x: Int, y: Int): Boolean = world.contains((x, y))

  def setCellStatus(x: Int, y: Int, status: Boolean): GameOfLife = {
    assert(x >= 0 && x < width, s"x position must be between 0 and ${width-1}")
    assert(y >= 0 && y < height, s"y position must be between 0 and ${height-1}")

    if (status) new GameOfLife(world + ((x, y)), width, height)
    else new GameOfLife(world - ((x, y)), width, height)
  }

  def next() = {
    val newWorld = new scala.collection.mutable.HashSet[GameOfLife.Cell]
    val deadCells = new scala.collection.mutable.HashSet[GameOfLife.Cell]  // dead cells with a least one live neighbour

    for (cell <- world) {
      val numberOfLiveNeighbours = getLiveNeighbours(cell).size
      deadCells ++= getDeadNeighbours(cell)

      if (numberOfLiveNeighbours == 2 || numberOfLiveNeighbours == 3) {
        newWorld += cell
      }
    }

    for(cell <- deadCells) {
      val numberOfLiveNeighbours = getLiveNeighbours(cell).size

      if (numberOfLiveNeighbours == 3) {
        newWorld += cell
      }
    }

    new GameOfLife(newWorld.toSet, width, height)
  }

  private def getLiveNeighbours(cell: GameOfLife.Cell) : Seq[GameOfLife.Cell] = {
    getNeighbours(cell).filter(world.contains(_))
  }

  private def getDeadNeighbours(cell: GameOfLife.Cell) : Seq[GameOfLife.Cell] = {
    getNeighbours(cell).filter(! world.contains(_))
  }

  private def getNeighbours(cell: GameOfLife.Cell) : Seq[GameOfLife.Cell] = {
    val neighbours = new ListBuffer[GameOfLife.Cell]

    for ( i <- -1 to 1) {
      for (j <- -1 to 1) if (i != 0 || j != 0) {
        val x = ((cell._1 + i) + width) % width
        val y = ((cell._2 + j) + height) % height

        neighbours += ((x, y))
      }
    }

    neighbours
  }
}
