package sigmastate.crypto

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.util
import java.util.{Arrays, Random}

class GF2_192_Specification extends AnyPropSpec
  with ScalaCheckDrivenPropertyChecks
  with Matchers {
  
  def assumeTrue(message: String, condition: Boolean) = {
    assume(condition, message)
  }
  
  def assumeFalse(message: String, condition: Boolean) = {
    assume(!condition, message)
  }
  
  private object GF2t_slow {
    def mulBits(ret: GF2t_slow, a: Array[Long], b: Array[Long]): Unit = {
      val c = new Array[Long](a.length + b.length)

      for ( i <- 0 until a.length ) {
        for ( i1 <- 0 until 64 ) {
          for ( j <- 0 until b.length ) {
            for ( j1 <- 0 until 64 ) {
              if ((a(i) & (1l << i1)) != 0 && (b(j) & (1l << j1)) != 0) {
                val cPosition = i * 64 + i1 + j * 64 + j1
                c(cPosition / 64) ^= 1l << (cPosition % 64)
              }
            }
          }
        }
      }
      ret.x = c
    }

    def modReduce(poly: GF2t_slow, mod: GF2t_slow.Modulus): Unit = {
      for (i <- poly.x.length * 64 - 1 to mod.degree by -1) {
        if ((poly.x(i >> 6) & (1l << (i & 63))) != 0) {
          for ( j <- 0 until mod.offset.length ) {
            val k = i - mod.offset(j)
            poly.x(k >> 6) ^= (1l << (k & 63))
          }
        }
      }
    }

    class Modulus private[crypto](val sparseModulus: Array[Int]) {
      // represented as an array of bit positions
      // where coefficient = 1, counting from degree down
      final private[crypto] var offset: Array[Int] = new Array[Int](sparseModulus.length)
      final private[crypto] var degree: Int = sparseModulus(0)

      offset(0) = 0
      for ( i <- 1 until sparseModulus.length ) {
        offset(i) = degree - sparseModulus(i)
      }
    }
  }

  private class GF2t_slow {
    var x: Array[Long] = null

    def isOne: Boolean = {
      if (x(0) != 1l) return false
      for ( i <- 1 until x.length ) {
        if (x(i) != 0l) return false
      }
      true
    }

    def equals(that: Array[Long]): Boolean = {
      var i = 0
      while ( i < Math.min(x.length, that.length) ) {
        if (x(i) != that(i)) return false
        i += 1
      }
      while ( i < x.length ) {
        if (x(i) != 0) return false
        i += 1
      }
      while ( i < that.length ) {
        if (that(i) != 0) return false
        i += 1
      }
      true
    }

    override def toString: String = {
      var ret = ""
      for ( i <- x.length - 1 to 0 by -1 ) {
        ret += x(i)
      }
      ret
    }
  }

  private var testValues: Array[Array[Long]] = null
  private val zero = new GF2_192(0)
  private val one = new GF2_192(1)
  private val pentanomial = Array(192, 7, 2, 1, 0)
  private val m = new GF2t_slow.Modulus(pentanomial)
  genTestValues()

  private def genTestValues(): Unit = {
    if (testValues == null) {
      testValues = new Array[Array[Long]](250)
      for ( i <- 0 until testValues.length ) {
        testValues(i) = new Array[Long](3)
      }

      // Test single 1s in every bit position but last
      // (1s in last bit position -- i.e., just the value of 1 -- will be tested separately)

      var j = 0
      var i = 1
      while ( i < 64 ) {
        testValues(j)(0) = 1L << i
        testValues(j)(1) = 0
        testValues(j)(2) = 0
        i += 1
        j += 1
      }
      i = 0
      while ( i < 64 ) {
        testValues(j)(0) = 0
        testValues(j)(1) = 1L << i
        testValues(j)(2) = 0
        i += 1
        j += 1
      }
      i = 0
      while ( i < 64 ) {
        testValues(j)(0) = 0
        testValues(j)(1) = 0
        testValues(j)(2) = 1L << i
        i += 1
        j += 1
      }

      // Test first word zero, last two words random,
      // and first word random, last two words 0
      // and first word random, second word 1, last word 0
      // and last word random, first two words 0
      val rand = new Random

      i = 0
      while ( i < 5 ) {
        testValues(j)(0) = 0
        testValues(j)(1) = rand.nextLong
        testValues(j)(2) = rand.nextLong
        i += 1
        j += 1
      }
      i = 0
      while ( i < 5 ) {
        testValues(j)(0) = rand.nextLong
        testValues(j)(1) = 0
        testValues(j)(2) = 0
        i += 1
        j += 1
      }
      i = 0
      while ( i < 5 ) {
        testValues(j)(0) = rand.nextLong
        testValues(j)(1) = 1
        testValues(j)(2) = 0
        i += 1
        j += 1
      }

      i = 0
      while ( i < 5 ) {
        testValues(j)(0) = 0
        testValues(j)(1) = 1
        testValues(j)(2) = rand.nextLong
        i += 1
        j += 1
      }
      // Test all three words random
      while ( j < testValues.length ) {
        testValues(j)(0) = rand.nextLong
        testValues(j)(1) = rand.nextLong
        testValues(j)(2) = rand.nextLong
        j += 1
      }
    }
  }

  property("constructorAndEqualityTest") {
    var t = new GF2_192
    var r = t.toLongArray
    assumeFalse("Fail: empty constructor.", !t.isZero || r.length != 3 || r(0) != 0L || r(1) != 0L || r(2) != 0L)

    t = new GF2_192(0)
    r = t.toLongArray
    assumeFalse("Fail: constructor on 0 int", !t.isZero || r.length != 3 || r(0) != 0L || r(1) != 0L || r(2) != 0L)

    t = new GF2_192(1)
    r = t.toLongArray
    assumeFalse("Fail: constructor on 1 int", !t.isOne || r.length != 3 || r(0) != 1L || r(1) != 0L || r(2) != 0L)

    t = new GF2_192(-1)
    r = t.toLongArray
    assumeFalse("Fail: constructor on 0xFFFFFFFF int " + t, r(0) != 0xFFFFFFFFL || r(1) != 0L || r(2) != 0L)

    var s = new Array[Long](3)

    s(0) = 123345L
    s(1) = 123567891234567L
    s(2) = 487237823242367L

    t = new GF2_192(s)
    var t1 = new GF2_192(t)

    r = t.toLongArray
    assumeFalse("Fail: constructor on long array", r(0) != s(0) || r(1) != s(1) || r(2) != s(2))

    r = t1.toLongArray
    assumeFalse("Fail: copy constructor", r(0) != s(0) || r(1) != s(1) || r(2) != s(2))

    val b = new Array[Byte](24)
    for ( i <- 0 until 8 ) {
      b(i) = (r(0) >>> (i * 8)).toByte
    }
    for ( i <- 0 until 8 ) {
      b(i + 8) = (r(1) >>> (i * 8)).toByte
    }
    for ( i <- 0 until 8 ) {
      b(i + 16) = (r(2) >>> (i * 8)).toByte
    }

    t = new GF2_192(b)
    s = t.toLongArray
    assumeFalse("Fail: constructor on byte array",
      r(0) != s(0) || r(1) != s(1) || r(2) != s(2))

    val c = t.toByteArray
    assumeFalse("Fail: toByteArray", !Arrays.equals(b, c))

    var b2 = new Array[Byte](30)
    for ( i <- 0 until 24 ) {
      b2(i + 6) = b(i)
    }

    t = new GF2_192(b2, 6)
    s = t.toLongArray
    assumeFalse("Fail: constructor on byte array with offset",
      r(0) != s(0) || r(1) != s(1) || r(2) != s(2))

    var b1 = t.toByteArray
    assumeFalse("Fail: toByteArray", !Arrays.equals(b, b1))

    var b3 = new Array[Byte](40)
    t.toByteArray(b3, 10)
    for ( i <- 0 until b.length ) {
      assumeFalse("Fail: toByteArray with offset",
        b3(i + 10) != b(i))
    }

    s(0) = 0xFFFFFFFFFFFFFFFFL
    s(1) = 0xFFFFFFFFFFFFFFFFL
    s(2) = 0xFFFFFFFFFFFFFFFFL
    t = new GF2_192(s)
    t1 = new GF2_192(t)

    r = t.toLongArray
    assumeFalse("Fail: constructor on long array of all 1s",
      r(0) != s(0) || r(1) != s(1) || r(2) != s(2))

    r = t1.toLongArray
    assumeFalse("Fail: copy constructor",
      r(0) != s(0) || r(1) != s(1) || r(2) != s(2))

    for ( i <- 0 until 8 ) {
      b(i) = (r(0) >>> (i * 8)).toByte
    }

    for ( i <- 0 until 8 ) {
      b(i + 8) = (r(1) >>> (i * 8)).toByte
    }

    for ( i <- 0 until 8 ) {
      b(i + 16) = (r(2) >>> (i * 8)).toByte
    }

    t = new GF2_192(b)
    s = t.toLongArray
    assumeFalse("Fail: constructor on byte array of all 1s",
      r(0) != s(0) || r(1) != s(1) || r(2) != s(2))

    b1 = t.toByteArray
    assumeFalse("Fail: toByteArray all 1s", !Arrays.equals(b, b1))

    b2 = new Array[Byte](30)
    for ( i <- 0 until 24 ) {
      b2(i + 6) = b(i)
    }
    t = new GF2_192(b2, 6)
    s = t.toLongArray
    assumeFalse("Fail: constructor on byte array with offset of all 1s",
      r(0) != s(0) || r(1) != s(1) || r(2) != s(2))

    b1 = t.toByteArray
    assumeFalse("Fail: toByteArray all 1s", !Arrays.equals(b, b1))

    b3 = new Array[Byte](40)
    t.toByteArray(b3, 10)
    for ( i <- 0 until b.length ) {
      assumeFalse("Fail: toByteArray all 1s with offset", b3(i + 10) != b(i))
    }
  }

  property("pow2To2ToKTest") {
    // includes squaring test
    val res = new GF2_192
    var z: GF2_192 = null
    val maxK = 15
    for ( k <- 0 until maxK ) {
      GF2_192.power2To2ToK(res, zero, k)
      assumeFalse("Fail: power2To2ToK of 0 for k=" + k, !res.isZero)

      z = new GF2_192(zero)
      GF2_192.power2To2ToK(z, z, k)
      assumeFalse("Fail: power2To2ToK of 0 in place for k=" + k, !z.isZero)

      GF2_192.power2To2ToK(res, one, k)
      assumeFalse("Fail: power2To2ToK of 1 for k=" + k, !res.isOne)

      z = new GF2_192(one)
      GF2_192.power2To2ToK(z, z, k)
      assumeFalse("Fail: power2To2ToK of 1 in place for k=" + k, !z.isOne)
    }

    GF2_192.sqr(res, zero)
    assumeFalse("Fail: sqr of 0", !res.isZero)

    z = new GF2_192(zero)
    GF2_192.sqr(z, z)
    assumeFalse("Fail: sqr of 0 in place", !z.isZero)

    GF2_192.sqr(res, one)
    assumeFalse("Fail: sqr of 1", !res.isOne)

    z = new GF2_192(one)
    GF2_192.sqr(z, z)
    assumeFalse("Fail: sqr of 1 in place", !z.isOne)

    val res1 = new GF2_192
    val res2 = new GF2_192
    for ( p <- testValues ) {
      for ( k <- 0 until maxK ) {
        z = new GF2_192(p)
        GF2_192.power2To2ToK(res, z, k)
        if (k == 0) {
          // Ground truth for squaring: self-multiply
          GF2_192.mul(res1, z, z) // sqr should equal power2To2ToK with k = 0
          assumeFalse("Fail: power2To2To1  " + z, !(res == res1))
          GF2_192.sqr(res2, z) // sqr should equal self-multiply with k = 0
          assumeFalse("Fail: sqr for k = " + k + " value = " + z, !(res == res2))
        }
        else {
          // res1 is the ground truth, computed using smaller values of k than is currently being tested
          GF2_192.power2To2ToK(res1, res1, k - 1)
          assumeFalse("Fail: power2To2ToK for k = " + k + " value = " + z, !(res == res1))
        }

        // Input location = output location tests
        GF2_192.power2To2ToK(z, z, k) // power2To2ToK into same location
        assumeFalse("Fail: power2To2ToK in place for k = " + k + " value = " + new GF2_192(p), !(res == z))
        if (k == 0) {
          z = new GF2_192(p)
          GF2_192.sqr(z, z) // sqr into same location
          assumeFalse("Fail: sqr in place " + new GF2_192(p), !(res == z))
        }
      }
    }
  }

  property("specialMultTest") {
    val res = new GF2_192
    val res1 = new GF2t_slow

    // Run everything times 0 and 0 times everything
    // and everything times 1 and 1 times everything
    // where 0 and 1 are GF2_192

    for ( p <- testValues ) {
      var p1 = new GF2_192(p)
      GF2_192.mul(res, p1, zero)
      assumeFalse("Fail: " + p1 + " * 0", !res.isZero)
      GF2_192.mul(p1, p1, zero)
      assumeFalse("Fail: " + p1 + " * 0" + " in place ", !p1.isZero)
      p1 = new GF2_192(p)
      GF2_192.mul(res, zero, p1)
      assumeFalse("Fail: 0 * " + p1, !res.isZero)
      GF2_192.mul(p1, zero, p1)
      assumeFalse("Fail: 0 * " + p1 + " in place ", !p1.isZero)
      p1 = new GF2_192(p)
      GF2_192.mul(res, p1, one)
      assumeFalse("Fail: " + p1 + " * 1", !(res == p1))
      GF2_192.mul(p1, p1, one)
      assumeFalse("Fail: " + p1 + " * 1 in place", !(res == p1))
      GF2_192.mul(res, one, p1)
      assumeFalse("Fail: 1 * " + p1, !(res == p1))
      GF2_192.mul(p1, one, p1)
      assumeFalse("Fail: 1 * " + p1 + " in place", !(res == p1))
    }

    // Run everything times 0
    // and everything times 1
    // where 0 and 1 are bytes
    for ( p <- testValues ) {
      val p1 = new GF2_192(p)
      GF2_192.mul(res, p1, 1.toByte)
      assumeFalse("Fail: " + p1 + " * 1 byte ", !(res == p1))
      GF2_192.mul(p1, p1, 1.toByte)
      assumeFalse("Fail: " + p1 + " * 1 byte in place", !(res == p1))
      GF2_192.mul(res, p1, 0.toByte)
      assumeFalse("Fail: " + p1 + " * 0 byte", !res.isZero)
      GF2_192.mul(p1, p1, 0.toByte)
      assumeFalse("Fail: " + p1 + " * 0 byte in place", !p1.isZero)
    }

    // Run everything times every byte
    val temp = new Array[Long](1)
    for ( p <- testValues ) {
      for ( i <- 2 until 256 ) {
        val p1 = new GF2_192(p)
        temp(0) = i
        GF2_192.mul(res, p1, i.toByte)
        GF2t_slow.mulBits(res1, p, temp)
        GF2t_slow.modReduce(res1, m)
        assumeFalse("Fail: " + p1 + " * " + i + " byte", !res1.equals(res.toLongArray))
        GF2_192.mul(p1, p1, i.toByte)
        assumeFalse("Fail: " + p1 + " * " + i + " byte in place", !(res == p1))
      }
    }
  }

  property("specialAddTest") {
    val res = new GF2_192

    // Run everything plus 0 and 0 plus everything
    // where 0 is GF2_192
    for ( p <- testValues ) {
      val p1 = new GF2_192(p)
      GF2_192.add(res, p1, zero)
      assumeFalse(s"Fail: $p1 + 0", !(res == p1))
      GF2_192.add(p1, p1, zero)
      assumeFalse(s"Fail: $p1 + 0 in place", !(res == p1))
      GF2_192.add(res, zero, p1)
      assumeFalse(s"Fail: 0 + $p1", !(res == p1))
      GF2_192.add(p1, zero, p1)
      assumeFalse(s"Fail: $p1 + 0 in place", !(res == p1))
    }
  }

  property("generalAddTest") {
    val res = new GF2_192
    val res1 = new GF2t_slow
    res1.x = new Array[Long](3)

    // Try everything plus everything in the test array
    for ( p <- testValues ) {
      var p1 = new GF2_192(p)
      for ( q <- testValues ) {
        val q1 = new GF2_192(q)
        GF2_192.add(res, p1, q1)
        res1.x(0) = p(0) ^ q(0)
        res1.x(1) = p(1) ^ q(1)
        res1.x(2) = p(2) ^ q(2)
        assumeFalse(s"Fail: $p1 + $q1 = $res not $res1",
          !res1.equals(res.toLongArray))

        GF2_192.add(p1, p1, q1)
        assumeFalse(s"Fail: $p1 + $q1 in place 1 ", !(res == p1))

        p1 = new GF2_192(p)
        GF2_192.add(q1, p1, q1)
        assumeFalse(s"Fail: $p1 + $q1 in place 2 ", !(res == q1))
      }
    }

    // Try everything plus self in the test array, both in place and not, and make sure you get zeros
    for ( p <- testValues ) {
      val p1 = new GF2_192(p)
      GF2_192.add(res, p1, p1)
      assumeFalse(s"Fail: $p1 + self", !res.isZero)

      GF2_192.add(p1, p1, p1)
      assumeFalse(s"Fail: $p1 self in place", !p1.isZero)
    }
  }

  property("generalMultTest") {
    val res = new GF2_192
    val res1 = new GF2t_slow

    // Now run everything times everything in the test array
    // TODO: speed this up
    for ( p <- testValues ) {
      var p1 = new GF2_192(p)
      for ( q <- testValues ) {
        val q1 = new GF2_192(q)
        GF2_192.mul(res, p1, q1)
        GF2t_slow.mulBits(res1, p, q)
        GF2t_slow.modReduce(res1, m)
        assumeFalse(s"Fail: $p1 * $q1", !res1.equals(res.toLongArray))

        GF2_192.mul(p1, p1, q1)
        assumeFalse(s"Fail: $p1 * $q1 in place 1 ", !(res == p1))

        p1 = new GF2_192(p)
        GF2_192.mul(q1, p1, q1)
        assumeFalse(s"Fail: $p1 * $q1 in place 2 ", !(res == q1))
      }
    }

    // Try everything times self in the test array, in place
    for ( p <- testValues ) {
      val p1 = new GF2_192(p)
      GF2_192.sqr(res, p1)
      GF2_192.mul(p1, p1, p1)
      assumeFalse(s"Fail: $p1 * self in place", !(res == p1))
    }
  }

  property("inversionTest") {
    val res = new GF2_192
    val res2 = new GF2_192
    val res1 = new GF2t_slow

    // Test inversion of 1
    GF2_192.invert(res, one)
    assumeFalse("Fail: inversion of 1", !res.isOne)

    // Test inversion of everything
    for ( p <- testValues ) {
      val p1 = new GF2_192(p)
      if (!p1.isZero) {
        GF2_192.invert(res, p1)
        GF2_192.mul(res2, p1, res)
        assumeFalse(s"Fail: inversion of $p1 self-test ", !res2.isOne)

        GF2t_slow.mulBits(res1, res.toLongArray, p)
        GF2t_slow.modReduce(res1, m)
        assumeFalse(s"Fail: inversion of $p1 GF2t_slow-test", !res1.isOne)

        GF2_192.invert(p1, p1)
        assumeFalse(s"Fail: inversion of $p1 in place ", !(p1 == res))
      }
    }
  }

  property("interpolateTest") {
    // Test for null inputs, arrays of unequal length, etc.
    val optArray: Array[GF2_192] = new Array[GF2_192](2)
    optArray(0) = null
    optArray(1) = new GF2_192(17)

    var res: GF2_192_Poly = null
    val rand: Random = new Random

    // Try with arrays of length 0
    var p = GF2_192_Poly.interpolate(
      new Array[Byte](0), new Array[GF2_192](0), new GF2_192(0))
    assumeFalse("Zero polynomial should be 0 at 0", !p.evaluate(0.toByte).isZero)
    assumeFalse("Zero polynomial should be 0 at 5", !p.evaluate(5.toByte).isZero)

    val val17 = new GF2_192(17)
    p = GF2_192_Poly.interpolate(new Array[Byte](0), new Array[GF2_192](0), val17)
    assumeFalse("Constant 17 polynomial should be 17 at 0", !(p.evaluate(0.toByte) == val17))
    assumeFalse("Constant 17 polynomial should be 17 at 5", !(p.evaluate(5.toByte) == val17))

    for ( len <- 1 until 100 ) {
      val points = new Array[Byte](len)
      val values = new Array[GF2_192](len)
      val temp = new Array[Byte](24)
      for ( i <- 0 until len ) {
        // generate a byte that is not equal to anything in the array nor 0
        var doLoop = true
        while (doLoop) {
          var b: Byte = 0
          do b = rand.nextInt.toByte while ( b == 0.toByte )
          var j: Int = 0
          var doBreak = false
          while (!doBreak && j < i) {
            if (b == points(j)) { // detected equality with something in the array
              doBreak = true
            } else {
              j += 1
            }
          }
          if (j == i) { // did not detect equality with anything in the array
            points(i) = b
            doLoop = false
          }
        }
      }
      for ( i <- 0 until len ) {
        rand.nextBytes(temp)
        values(i) = new GF2_192(temp)
      }

      res = GF2_192_Poly.interpolate(points, values, null)
      for ( i <- 0 until len ) {
        val t = res.evaluate(points(i))
        assumeFalse(s"Interpolation error on length = $len at input point number $i", !(t == values(i)))
      }
      rand.nextBytes(temp)
      val valueAt0 = new GF2_192(temp)
      res = GF2_192_Poly.interpolate(points, values, valueAt0)
      for ( i <- 0 until len ) {
        val t = res.evaluate(points(i))
        assumeFalse(s"Interpolation error on length =  $len at input point number $i(with optional 0)", !(t == values(i)))
      }
      val t = res.evaluate(0.toByte)
      assumeFalse(s"Interpolation error on length =  $len at input optional 0", !(t == valueAt0))

      val b = res.toByteArray(false)
      val t1 = GF2_192_Poly.fromByteArray(valueAt0.toByteArray, b)
      val b1 = t1.toByteArray(false)
      assumeFalse(
        s"To byte array round trip error ${util.Arrays.toString(b)} ${util.Arrays.toString(b1)}",
        !Arrays.equals(b, b1))

      val b2 = t1.toByteArray(true)
      assumeFalse("To byte array round trip error at coeff0",
        !Arrays.equals(valueAt0.toByteArray, Arrays.copyOfRange(b2, 0, 24)))
      assumeFalse("To byte array round trip error with coeff0 at later coeff",
        !Arrays.equals(b1, Arrays.copyOfRange(b2, 24, b2.length)))

      val b3 = t1.coeff0Bytes
      assumeFalse("To byte array round trip error on coeff0",
        !Arrays.equals(b3, valueAt0.toByteArray))
    }

    for ( len <- 1 until 100 ) {
      val points = new Array[Byte](len)
      val values = new Array[GF2_192](len)
      val temp = new Array[Byte](24)

      for ( i <- 0 until len ) {
        // generate a byte that is not equal to anything in the array (but may be 0)
        var okLoop = true
        while (okLoop) {
          val b = rand.nextInt.toByte
          var j = 0
          j = 0
          var doBreak = false
          while (!doBreak && j < i) {
            if (b == points(j)) {
              doBreak = true
            } else {
              j += 1
            }
          }
          if (j == i) {
            points(i) = b
            okLoop = false
          }
        }
      }
      for ( i <- 0 until len ) {
        rand.nextBytes(temp)
        values(i) = new GF2_192(temp)
      }

      res = GF2_192_Poly.interpolate(points, values, null)
      for ( i <- 0 until len ) {
        val t = res.evaluate(points(i))
        assumeFalse(s"Interpolation error on length =  $len $i(with 0 allowed but not additional)", !(t == values(i)))
      }

      for ( opt <- optArray ) {
        res = GF2_192_Poly.interpolate(null, values, opt)
        assumeFalse("Fail: interpolate should output null on points = null", res != null)
        res = GF2_192_Poly.interpolate(points, null, opt)
        assumeFalse("Fail: interpolate should output null on values =  null", res != null)
        res = GF2_192_Poly.interpolate(points, new Array[GF2_192](0), opt)
        assumeFalse("Fail: interpolate should output null on values of length 0", res != null)
        res = GF2_192_Poly.interpolate(new Array[Byte](0), values, opt)
        assumeFalse("Fail: interpolate should output null on points of length 0", res != null)
        res = GF2_192_Poly.interpolate(new Array[Byte](len - 1), values, opt)
        assumeFalse("Fail: interpolate should output null on not enough points", res != null)
        res = GF2_192_Poly.interpolate(new Array[Byte](len + 1), values, opt)
        assumeFalse("Fail: interpolate should output null on too many points", res != null)
      }
    }
    for ( opt <- optArray ) {
      res = GF2_192_Poly.interpolate(null, null, opt)
      assumeFalse("Fail: interpolate should output null on both points and values = null", res != null)
    }
  }

  property("GF2_192 rejects short byte arrays with IllegalArgumentException") {
    an[IllegalArgumentException] should be thrownBy {
      new GF2_192(new Array[Byte](23), 0)
    }
    an[IllegalArgumentException] should be thrownBy {
      new GF2_192(new Array[Byte](24), 1)
    }
    an[IllegalArgumentException] should be thrownBy {
      new GF2_192(1).toByteArray(new Array[Byte](23), 0)
    }
  }
}
