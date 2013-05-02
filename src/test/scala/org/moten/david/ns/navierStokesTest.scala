package org.moten.david.ns

import _root_.org.junit._
import Direction._
import Assert._
import scala.math._
import scala.util.Random._
import Logger._

@Test
class VectorTest {

  val Precision = 0.000001

  @Test
  def testVectorAddition() {
    val v1 = new Vector(1, 2, 3)
    val v2 = new Vector(10, 20, 30)
    assertEquals(Vector(11, 22, 33), v1 add v2)
  }

  @Test
  def testVectorSubtraction() {
    val v1 = new Vector(1, 2, 3)
    val v2 = new Vector(10, 20, 30)
    assertEquals(Vector(9, 18, 27), v2 minus v1)
    assertEquals(Vector(9, 18, 27), v2 - v1)
  }

  @Test
  def testVectorDotProduct() {
    val v1 = new Vector(1, 2, 3)
    val v2 = new Vector(10, 20, 30)
    assertEquals(140.0, v1 * v2, Precision)
  }

  @Test
  def testVectorScalaProduct() {
    val v1 = new Vector(1, 2, 3)
    assertEquals(Vector(2, 4, 6), v1 * 2)
  }

  @Test
  def testVectorScalaDivide() {
    val v1 = new Vector(1, 2, 3)
    assertEquals(Vector(1.0 / 2, 2 / 2.0, 3 / 2.0), v1 / 2)
  }

  @Test
  def testVectorSum() {
    val v1 = new Vector(1, 2, 3.5)
    assertEquals(1 + 2 + 3.5, v1.sum, Precision)
  }

  @Test
  def testVectorModify() {
    val v1 = new Vector(1, 2, 3)
    assertEquals(Vector(4, 2, 3), v1.modify(X, 4))
    assertEquals(Vector(1, 4, 3), v1.modify(Y, 4))
    assertEquals(Vector(1, 2, 4), v1.modify(Z, 4))
  }
}

@Test
class NewtonsMethodTest {

  @Test
  def testNewtonsMethodSolvesANonLinearEquation() {
    def f(x: Double) = sqrt(x) - 3
    NewtonsMethod.solve(f, 2, 0.1, 0.0001, 5) match {
      case None => fail
      case x: Some[Double] => assertEquals(9.0, x.get, 0.001)
    }
  }

  @Test
  def testNewtonsMethodSolvesLinearInOneIteration() {
    def f(x: Double) = 2 * x - 3
    NewtonsMethod.solve(f, 2, 0.1, 0.0001, 1) match {
      case None => fail
      case x: Some[Double] => assertEquals(1.5, x.get, 0.001)
    }
  }

  @Test
  def testNewtonsMethodSolvesInOneIterationIfSuppliedWithAnswerAsInitialValue() {
    def f(x: Double) = sqrt(x) - 3
    NewtonsMethod.solve(f, 9, 0.1, 0.0001, 1) match {
      case None => fail
      case x: Some[Double] => assertEquals(9.0, x.get, 0.001)
    }
  }

  @Test
  def testNewtonsMethodReturnsNoneIfMaxIterationsIsZeroAndIntialValueIsWrong() {
    def f(x: Double) = sqrt(x) - 3
    NewtonsMethod.solve(f, 8, 0.1, 0.0001, 0) match {
      case None => Unit
      case x: Some[Double] => fail
    }
  }
  @Test
  def testNewtonsMethodReturnsCorrectAnswerIfMaxIterationsIsZeroAndIntialValueIsCorrect() {
    def f(x: Double) = sqrt(x) - 3
    NewtonsMethod.solve(f, 9, 0.1, 0.0001, 0) match {
      case None => fail
      case x: Some[Double] => assertEquals(9.0, x.get, 0.001)
    }
  }
}

object Util {

  case class Pos(x: Double, y: Double, z: Double) extends HasPosition {
    val position = Vector(x, y, z)
  }

  def vectors(size: Int) = {
    val range = Range(1, size + 1)
    (for (
      i <- range;
      j <- range;
      k <- range
    ) yield (i, j, k))
      .map(t => Vector(t._1, t._2, -t._3))
  }

  def vectors2D(size: Int) = {
    val range = Range(0, size + 1)
    (for (
      i <- range;
      j <- range;
      k <- Range(0, 3)
    ) yield (i, j, k))
      .map(t => Vector(1.0 * t._1 / size, 1.0 * t._2 / size, -t._3))
  }

  def addBoundary(positions: Set[HasPosition]): Set[HasPosition] = {

    val extras =
      RegularGrid.getPositionsByTwoOrdinatesAndDirection(positions)
        .toList
        .map(x => {
          val direction = x._1._2
          val list = x._2
          val headExtras =
            list.head match {
              case c: EdgeCandidate => Set.empty[HasPosition]
              case _ => {
                val diff = if (list.length > 1)
                  list(1).position.get(direction) - list(0).position.get(direction)
                else
                  1
                Set[HasPosition](Obstacle(list.head.position.add(direction, -diff)))
              }
            }
          val tailExtras =
            list.tail match {
              case c: EdgeCandidate => Set.empty[HasPosition]
              case _ => {
                val diff = if (list.length > 1)
                  list.last.position.get(direction) - list(list.length - 2).position.get(direction)
                else
                  1
                Set[HasPosition](Obstacle(list.last.position.add(direction, diff)))
              }
            }
          headExtras ++ tailExtras
        }).flatten

    positions ++ extras
  }
}

@Test
class UtilTest {

  import Util._

  @Test
  def testAddBoundaryReturnsEmptySetGivenEmptySet {
    val set = addBoundary(Set[HasPosition]())
    assertTrue(set.isEmpty)
  }

  @Test
  def testAddBoundaryReturnsBoundaryGivenSingletonSet {
    val set = addBoundary(Set[HasPosition](Pos(1, 1, 1)))
    info("singleton set with boundary=" + set)
    assertEquals(7, set.size)
    assertTrue(set.contains(Obstacle(Vector(1, 1, 0))))
    assertTrue(set.contains(Obstacle(Vector(1, 1, 2))))
    assertTrue(set.contains(Obstacle(Vector(1, 0, 1))))
    assertTrue(set.contains(Obstacle(Vector(1, 2, 1))))
    assertTrue(set.contains(Obstacle(Vector(0, 1, 1))))
    assertTrue(set.contains(Obstacle(Vector(2, 1, 1))))
    assertTrue(set.contains(Pos(1, 1, 1)))
  }

  @Test
  def testAddBoundaryReturnsBoundaryGivenTwoPointsSet {
    val set = addBoundary(Set[HasPosition](Pos(1, 1, 1), Pos(3, 1, 1)))
    info("two points set with boundary=" + set)
    assertEquals(12, set.size)

    assertTrue(set.contains(Obstacle(Vector(-1, 1, 1))))
    assertTrue(set.contains(Obstacle(Vector(5, 1, 1))))

    assertTrue(set.contains(Obstacle(Vector(1, 1, 0))))
    assertTrue(set.contains(Obstacle(Vector(1, 1, 2))))
    assertTrue(set.contains(Obstacle(Vector(1, 0, 1))))
    assertTrue(set.contains(Obstacle(Vector(1, 2, 1))))

    assertTrue(set.contains(Obstacle(Vector(3, 1, 0))))
    assertTrue(set.contains(Obstacle(Vector(3, 1, 2))))
    assertTrue(set.contains(Obstacle(Vector(3, 0, 1))))
    assertTrue(set.contains(Obstacle(Vector(3, 2, 1))))

    assertTrue(set.contains(Pos(1, 1, 1)))
    assertTrue(set.contains(Pos(3, 1, 1)))
  }

}

@Test
class GridDataTest {
  import Util._
  import Vector._
  import RegularGrid._
  import Sign._
  import Throwing._

  @Test
  def testGetDirectionalNeighbours() {
    val positions = addBoundary(Set[HasPosition](Pos(1, 1, 1), Pos(2, 1, 1), Pos(3, 1, 1)))
    val n = RegularGrid(positions).neighbours _
    assertEquals(Pos(2.0, 1.0, 1.0), n(X, Negative, Vector(3.0, 1.0, 1.0)))
    assertEquals(Pos(2.0, 1.0, 1.0), n(X, Positive, Vector(1.0, 1.0, 1.0)))
  }

  @Test
  def testClosestNeighbourInPositiveDirectionFromLeftEndReturnsNextInList() {
    val list = List[HasPosition](Pos(1, 1, 1), Pos(2, 1, 1), Pos(3, 1, 1))
    val p = RegularGrid.closestNeighbour(list, X, Positive, Vector(1, 1, 1))
    assertEquals(Pos(2, 1, 1), p)
  }

  @Test
  def testClosestNeighbourInPositiveDirectionFromRightEndReturnsEmpty() {
    val list = List[HasPosition](Pos(1, 1, 1), Pos(2, 1, 1), Pos(3, 1, 1))
    val p = RegularGrid.closestNeighbour(list, X, Positive, Vector(3, 1, 1))
    assertTrue(p.isInstanceOf[Empty])
  }

  @Test
  def testClosestNeighbourInNegativeDirectionFromRightEndReturnsPreviousInList() {
    val list = List[HasPosition](Pos(1, 1, 1), Pos(2, 1, 1), Pos(3, 1, 1))
    val p = RegularGrid.closestNeighbour(list, X, Negative, Vector(3, 1, 1))
    assertEquals(Pos(2, 1, 1), p)
  }

  @Test
  def testClosestNeighbourInNegativeDirectionFromLeftEndReturnsEmpty() {
    val list = List[HasPosition](Pos(1, 1, 1), Pos(2, 1, 1), Pos(3, 1, 1))
    val p = RegularGrid.closestNeighbour(list, X, Negative, Vector(1, 1, 1))
    assertTrue(p.isInstanceOf[Empty])
  }

  @Test
  def testDirectionalNeighboursWithOneZLayerOnly() {
    //TODO
  }

  @Test
  def checkDirectionalNeighboursReturnsEmptyMapOnGivenEmptySet() {
    assertEquals(Map(), getDirectionalNeighbours(Set()))
  }

  @Test
  def checkDirectionalNeighboursReturns3DirectionsTimes2SignsGivenOnePointSet() {
    assertEquals(6, getDirectionalNeighbours(Set(Empty(Vector(0, 0, 0)))).size)
  }
}

@Test
class NavierStokesTest {
  import Util._
  import Throwing._

  @Test
  def testNavierStokesStepDoesNothingToDataInEquilibrium() {
    info("equilibrium run")
    //create a 5x5x5 regular grid with no movement and 0 surface pressure, 
    //seawater kinematic viscosity is for 20 degrees C
    val size = 20
    info("creating positions")
    val positions: Set[HasPosition] =
      addBoundary(
        List(Vector(1, 1, 1))
          .par
          .map(v => Value(
            v,
            velocity = Vector.zero,
            pressure = abs(v.z * 1000 * 9.8),
            density = 1000,
            viscosity = 0.00000105))
          .seq.toSet)

    info("creating Data")
    val data = new RegularGridSolver(positions)
    //    println(data)
    val data2 = data.step(30)
    //    println(data2)
    //should be no change in any value after 30 steps
    data.getPositions.foreach(p => println(p))
  }

  private def equals(v1: Value, v2: Value, precision: Double): Boolean = {
    abs(v1.pressure - v2.pressure) <= precision && equals(v1.velocity, v2.velocity, precision)
  }

  private def equals(v1: Vector, v2: Vector, precision: Double) = {
    abs(v1.x - v2.x) <= precision && abs(v1.y - v2.y) <= precision && abs(v1.z - v2.z) <= precision
  }
}
