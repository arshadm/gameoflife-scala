package org.arshadm.gameoflife

import scala.collection.immutable.HashSet
import scala.collection.mutable.ListBuffer

object GameOfLife {
  type Cell = Product2[Int, Int]
}

class GameOfLife (val world: Set[GameOfLife.Cell] = Set.empty, val width: Int = 100, val height: Int = 100) {
  def getCellStatus(x: Int, y: Int): Boolean = world.contains((x, y))

  def setCellStatus(x: Int, y: Int, status: Boolean): GameOfLife =
    if (status) new GameOfLife(world + ((x, y)), width, height)
    else new GameOfLife(world - ((x, y)), width, height)

  def next() = {
    val newWorld = new scala.collection.mutable.HashSet[GameOfLife.Cell]
    val deadCells = new scala.collection.mutable.HashSet[GameOfLife.Cell]  // dead cells with a least one live neighbour

    for (cell <- world) {
      val numberOfLiveNeighbours = getLiveNeighbours(cell).size
      deadCells ++= getDeadNeighbours(cell)

      if (numberOfLiveNeighbours < 2 || numberOfLiveNeighbours > 3) {
        /* Cell does not survive into next iteration */
      } else if (numberOfLiveNeighbours == 2 || numberOfLiveNeighbours == 3) {
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

  def getLiveNeighbours(cell: GameOfLife.Cell) : Seq[GameOfLife.Cell] = {
    getNeighbours(cell).filter(world.contains(_))
  }

  def getDeadNeighbours(cell: GameOfLife.Cell) : Seq[GameOfLife.Cell] = {
    getNeighbours(cell).filter(! world.contains(_))
  }

  def getNeighbours(cell: GameOfLife.Cell) : Seq[GameOfLife.Cell] = {
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
