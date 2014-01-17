
package maqicode.testing

import scala.reflect.ClassTag

trait UnitTest {
  def throws[A <: Throwable : ClassTag](body: => Any): Unit =
    try body catch { case e: A => }
}

trait JUnitTest extends UnitTest with JUnitAssertions {

  /*
  implicit class Averrable(val b: Boolean) extends AnyVal {
    def yes = assert(b)
    def no = assert(!b)
  }
  */
  //private[this] val constFalse: Any => Boolean = { _ => false}
  //def aver[A](x: A)(pf: PartialFunction[A, Boolean]): Unit = assertTrue(condition = pf.applyOrElse(x, constFalse))
  //def deny[A](x: A)(pf: PartialFunction[A, Boolean]): Unit = assertFalse(condition = pf.applyOrElse(x, constFalse))
  import PartialFunction.cond
  def aver[A](x: A)(pf: PartialFunction[A, Boolean]): Unit = assertTrue(condition = cond(x)(pf))
  def deny[A](x: A)(pf: PartialFunction[A, Boolean]): Unit = assertFalse(condition = cond(x)(pf))
}

/** The well-known static members of JUnit's `Assert`, as a mix-in,
 *  and the optional `message` parameter last.
 */
trait JUnitAssertions {
  import org.junit.Assert

  def assertTrue(condition: Boolean, message: String = null)  = Assert.assertTrue(message, condition)
  def assertFalse(condition: Boolean, message: String = null) = Assert.assertFalse(message, condition)

  def fail(message: String = null) = Assert.fail(message)

  def assertEquals(expected: Any, actual: Any, message: String = null)                      = Assert.assertEquals(message, expected, actual)
  def assertEquals(expected: Long, actual: Long, message: String)                           = Assert.assertEquals(message, expected, actual)
  def assertEquals(expected: Long, actual: Long)                                            = Assert.assertEquals(null, expected, actual)
  def assertEquals(expected: Double, actual: Double, delta: Double, message: String)        = Assert.assertEquals(message, expected, actual, delta)
  def assertEquals(expected: Double, actual: Double, delta: Double)                         = Assert.assertEquals(null, expected, actual, delta)
  def assertEquals(expected: Float, actual: Float, delta: Float, message: String)           = Assert.assertEquals(message, expected, actual, delta)
  def assertEquals(expected: Float, actual: Float, delta: Float)                            = Assert.assertEquals(null, expected, actual, delta)

  /* 4.12
  def assertArrayEquals(expecteds: Array[Boolean], actuals: Array[Boolean], message: String) =
    Assert.assertArrayEquals(message, expecteds, actuals)
  def assertArrayEquals(expecteds: Array[Boolean], actuals: Array[Boolean]) =
    Assert.assertArrayEquals(null, expecteds, actuals)
  */
  def assertArrayEquals(expecteds: Array[Int], actuals: Array[Int], message: String) =
    Assert.assertArrayEquals(message, expecteds, actuals)
  def assertArrayEquals(expecteds: Array[Int], actuals: Array[Int]) =
    Assert.assertArrayEquals(null, expecteds, actuals)
  def assertArrayEquals(expecteds: Array[Long], actuals: Array[Long], message: String) =
    Assert.assertArrayEquals(message, expecteds, actuals)
  def assertArrayEquals(expecteds: Array[Long], actuals: Array[Long]) =
    Assert.assertArrayEquals(null, expecteds, actuals)
  def assertArrayEquals(expecteds: Array[Double], actuals: Array[Double], delta: Double, message: String) =
    Assert.assertArrayEquals(message, expecteds, actuals, delta)
  def assertArrayEquals(expecteds: Array[Double], actuals: Array[Double], delta: Double) =
    Assert.assertArrayEquals(null, expecteds, actuals, delta)
  def assertArrayEquals(expecteds: Array[Float], actuals: Array[Float], delta: Float, message: String) =
    Assert.assertArrayEquals(message, expecteds, actuals, delta)
  def assertArrayEquals(expecteds: Array[Float], actuals: Array[Float], delta: Float) =
    Assert.assertArrayEquals(null, expecteds, actuals, delta)

  /* 4.11
  def assertNotEquals(unexpected: Any, actual: Any, message: String = null)               = Assert.assertNotEquals(message, unexpected, actual)
  def assertNotEquals(unexpected: Long, actual: Long, message: String)                    = Assert.assertNotEquals(message, unexpected, actual)
  def assertNotEquals(unexpected: Long, actual: Long)                                     = Assert.assertNotEquals(null, unexpected, actual)
  def assertNotEquals(unexpected: Double, actual: Double, delta: Double, message: String) = Assert.assertNotEquals(message, unexpected, actual, delta)
  def assertNotEquals(unexpected: Double, actual: Double, delta: Double)                  = Assert.assertNotEquals(null, unexpected, actual, delta)
  def assertNotEquals(unexpected: Float, actual: Float, delta: Float, message: String)    = Assert.assertNotEquals(message, unexpected, actual, delta)
  def assertNotEquals(unexpected: Float, actual: Float, delta: Float)                     = Assert.assertNotEquals(null, unexpected, actual, delta)
  */

  def assertNull(objet: Any, message: String = null)    = Assert.assertNull(message, objet)
  def assertNotNull(objet: Any, message: String = null) = Assert.assertNotNull(message, objet)

  def assertSame(expected: Any, actual: Any, message: String = null)      = Assert.assertSame(message, expected, actual)
  def assertNotSame(unexpected: Any, actual: Any, message: String = null) = Assert.assertNotSame(message, unexpected, actual)
}
