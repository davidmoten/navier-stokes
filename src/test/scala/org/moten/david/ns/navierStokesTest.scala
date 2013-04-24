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
}

@Test
class GridDataTest {
  import Util._
  import Vector._
  import RegularGrid._
  import Sign._
  import Throwing._

  case class Pos(x: Double, y: Double, z: Double) extends HasPosition {
    val position = Vector(x,y,z)
  }

  @Test
  def testGetDirectionalNeighbours() {
	val positions=Set[HasPosition](Pos(1,1,1),Pos(2,1,1),Pos(3,1,1))
	val n = RegularGrid(positions).neighbours
	println(n)
	assertEquals(Pos(2.0,1.0,1.0),n.getOrElse((X,Negative,Pos(3.0,1.0,1.0)),unexpected))
	assertEquals(Pos(2.0,1.0,1.0),n.getOrElse((X,Positive,Pos(1.0,1.0,1.0)),unexpected))
  }
  
  @Test
  def testClosestNeighbour() {
  
    val list=List[HasPosition](Pos(1,1,1),Pos(2,1,1),Pos(3,1,1))
    
    val p = RegularGrid.closestNeighbour(list,X,Positive,Pos(1,1,1))
    assertEquals(2.0,p.position.x,0.0001)
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
  def checkDirectionalNeighboursReturnsEmptyMapOnGivenOnePointSet() {
    //    assertEquals(Map(), getDirectionalNeighbours(Set(zero)))
  }
}

